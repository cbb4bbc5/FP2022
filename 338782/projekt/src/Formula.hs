
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
module Formula(
        Term(),substInTerm,freeInTerm,
        Formula(),FormulaId(..),formulaId,
        freeInFormula,freeInFormulas,substInFormula,allVars,freshVars,freshVarsInFormulas,
        substManyInTerm,substManyInFormula,getFormulaDiff,matchFormulas,
        makeVar,makeSym,makeRelation,
        freeVar,forallFormula,existsFormula,
        getTermDiff,getRenameTerm,getRenameFormula,
        andFormula,orFormula,impliesFormula,iffFormula,notFormula,topFormula,bottomFormula,
        readTerm,readFormula,
        Format(..),AnyFormat(..),ListState(..),anyFormat,AnyFill(..),substFormat,cleanUpFormula,
        sprintfF,sprintfT,evalFormat,MonadInput(..)) where
    import Data.Maybe
    import qualified Data.Map as Map
    import qualified Data.List as List
    import Control.Monad
    import Control.Monad.IO.Class
    import Control.Monad.Trans.State( evalStateT, runStateT,  StateT )
    import Control.Monad.Trans.Accum(AccumT)
    import Control.Monad.Trans.Reader(ReaderT)
    import Control.Monad.State.Class
    import Control.Monad.Trans.Class(MonadTrans(..))
    import Data.Bifunctor
    import Data.Functor
    import Control.Applicative
    import Text.Read(readPrec,Lexeme(..),lexP,parens,(+++))
    
    operatorChars :: String
    operatorChars = "!@#$%^&*:|.?-=+<>/~" :: String
    data VarName = VarName Char Int deriving (Eq,Ord)
    instance Show VarName where
        show (VarName c i) = c : if i>0 then show i else []
    instance Enum VarName where
        fromEnum (VarName c i) = 26*i + (fromEnum c - fromEnum 'a')
        toEnum n = VarName (toEnum $ fromEnum 'a' + mod n 26) (div n 26)

    data Var = LocalVar Int | FreeVar String deriving (Eq,Ord)
    instance Show Var where
        show (FreeVar s) = s
        show (LocalVar i) = show i
    type Sym = String

    parenSplit :: String -> [(String,String)]
    parenSplit (' ':xs) = parenSplit xs
    parenSplit s@('"':_) = reads s
    parenSplit s@('(':_) = let (x,y) = findEnd 0 (s,"") in [(reverse y,x)]
        where
            findEnd :: Int -> (String,String) -> (String,String)
            findEnd _ ([],ys) = ([],ys)
            findEnd n ('(':xs,ys) = findEnd (n+1) (xs,'(':ys)
            findEnd 1 (')':xs,ys) = (xs,')':ys)
            findEnd n (')':xs,ys) = findEnd (n-1) (xs,')':ys)
            findEnd n (x:xs,ys) = findEnd n (xs,x:ys)
    parenSplit s = [(s,"")]

    data Term = Var Var | Sym Sym [Term] deriving (Eq,Ord)
    instance Show Term where
        showsPrec _ (Var v) s = shows v s
        showsPrec 0 (Sym sym xs) s  | all (`elem` ['0'..'9'] ) sym && null xs = sym ++ s
                                    | all (`elem` operatorChars) sym = if length xs == 2 && not (null sym) && notElem sym ["->","|","&"] 
                                                                then let y1 = showsPrec (if unsym (head xs) then 11 else 0) (head xs) ""
                                                                         y2 = showsPrec (if unsym (xs!!1) then 11 else 0) (xs!!1) ""
                                                                        in
                                                                    if Sym "" [] `elem` xs
                                                                        then y1 ++ sym ++ y2 ++ s
                                                                        else y1++" "++sym++" "++y2 ++ s
                                                                else 
                                                                    if null sym then "("++commas xs++")" ++ s
                                                                                else "("++sym++")("++commas xs++")" ++ s
                                    | otherwise = sym ++ "(" ++ commas xs ++ ")"
            where 
                commas :: [Term] -> String
                commas [] = []
                commas [x1] = show x1
                commas (x1:xs1) = show x1++", "++commas xs1

                unsym :: Term -> Bool
                unsym (Var _) = False
                unsym (Sym sym1 _) = not (null sym1) && all (`elem` operatorChars) sym1 && length xs == 2 && notElem sym ["->","|","&"]
        showsPrec n t s | n<=6, Sym sym [a,b] <- t,not (null sym), all (`elem` operatorChars) sym, Sym "" [] `elem` [a,b] =  "(" ++ shows t ")" ++ s
                        | n<=6 = shows t s
                        | otherwise =  "(" ++ shows t ")" ++ s
        
    readStateTerm :: (MonadInput m,MonadState (AnyFormat,[AnyFormat]) m, MonadPlus m) => m (Term -> [Term])
    readStateTerm = do
        (xs0,xsF) <- get
        case xs0 of
            TermA -> do
                t <- input TermF
                f <- case List.uncons xsF of Nothing -> return (:[]); Just xfs -> put xfs >> readStateTerm
                return $ \_ -> f t
            StringA  -> input StringF >>= put . (,xsF) . LitA . filter (/=' ') >> readStateTerm
            FormulaA -> mzero
            FormatA -> inputFormat >>= put . (,xsF) >> readStateTerm
            x :^^: y -> put (x,y : xsF) >> readStateTerm
            LitA xs -> case xs of
                [] -> case xsF of
                        [] -> return (:[])
                        (xf:xsf) -> put (xf,xsf) >> readStateTerm
                (',':xs1) -> do
                    put (LitA xs1, xsF)
                    f <- readStateTerm
                    return $ flip (:) $ f $ Sym "" []
                (')':xs1) -> put (LitA xs1, xsF) >> return (:[])
                ('}':xs1) -> put (LitA xs1, xsF) >> return (:[])
                ('{':xs1) -> do
                    put (LitA xs1, xsF)
                    f <- readStateTerm
                    f1 <- readStateTerm
                    let subst = Map.fromList $ mapMaybe (\case (Sym _  [Var (FreeVar s),t]) -> Just (s,t); _ -> Nothing) $ f $ Sym "" []
                    return $ f1 . substManyInTerm subst
                (_:_) ->
                    let (sym,xs1) = span (`notElem` operatorChars++"(){},") xs in
                        let (sym1,xs11) = span (/=')') $ if null xs1 then [] else tail xs1 in
                            if null sym && not (null sym1) && all (`elem` operatorChars) sym1 && take 2 xs11 == ")("
                                then 
                                    do
                                        put (LitA $ drop 2 xs1,xsF)
                                        f <- readStateTerm
                                        f1 <- readStateTerm
                                        return (\_ -> f1 $ Sym sym1 $ f $ Sym "" [])
                                else let (symO,xsO) = span (`elem` operatorChars) xs in
                                if not (null symO)
                                    then do
                                        put (LitA xsO,xsF)
                                        f <- readStateTerm
                                        let pnxs = f $ Sym "" [] 
                                        return $ \x -> Sym symO [x, head pnxs]:tail pnxs
                                    else 
                                        if null xs1 || head xs1 /= '('
                                            then do 
                                                put (LitA xs1,xsF)
                                                f <- readStateTerm
                                                return (\_ -> f $ varOrNum sym)
                                            else do
                                                put (LitA $ tail xs1,xsF)
                                                f <- readStateTerm
                                                f1 <- readStateTerm
                                                return (\_ -> f1 $ Sym sym $ f $ Sym "" [])
        where
            varOrNum :: String -> Term
            varOrNum sym = if all (`elem` ['0'..'9']) sym then Sym sym []
                                                          else Var (FreeVar sym)
    cleanUpTerm :: Term -> Term
    cleanUpTerm var@(Var _) = var
    cleanUpTerm (Sym "" [t]) = cleanUpTerm t
    cleanUpTerm (Sym sym xs) = Sym sym $ map cleanUpTerm xs

    toFormats :: String -> (AnyFormat,[AnyFormat])
    toFormats = (,[]) . LitA . filter (/=' ')

    cleanUpFormat :: (AnyFormat,[AnyFormat]) -> (AnyFormat,[AnyFormat])
    cleanUpFormat (x0,xs) = (cleanUpSingle x0,map cleanUpSingle xs)
        where
            cleanUpSingle :: AnyFormat -> AnyFormat
            cleanUpSingle (LitA s) = LitA $ filter (/=' ') s
            cleanUpSingle (x :^^: y) = cleanUpSingle x :^^: cleanUpSingle y
            cleanUpSingle f = f

    readTerm :: (MonadInput m, MonadState (AnyFormat,[AnyFormat]) m, MonadPlus m) => m Term
    readTerm = get >>= put . cleanUpFormat >> readStateTerm <&> cleanUpTerm . head . ($ Sym "" [])

    instance Read Term where
        readsPrec 0 t = maybeToList $ do
                                        (term,(LitA s1,_)) <- unNoInput $ runStateT readTerm $ toFormats t
                                        guard $ term /= Sym "" []
                                        return (term,s1)
        readsPrec _ t = do (s,rest) <- parenSplit t
                           map (second (++rest)) $ reads s

                
    data Player = Universal | Existential deriving (Eq,Ord,Show)
    data Formula = 
          Relation Sym [Term]
        | Quant Player Formula
        | Choice Player Formula Formula
        | Implies Formula Formula
        | Iff Formula Formula 
        | Not Formula
        | Bottom
        | Top
        | Paren Formula
        deriving (Eq,Ord)
    data FormulaId = 
          RelationFormula Sym [Term] 
        | ForallFormula 
        | ExistsFormula 
        | AndFormula Formula Formula
        | OrFormula Formula Formula
        | ImpliesFormula Formula Formula
        | IffFormula Formula Formula
        | NotFormula Formula
        | BottomFormula 
        | TopFormula
        deriving (Eq,Ord,Show)
    formulaId :: Formula -> FormulaId
    formulaId (Relation sym xs) = RelationFormula sym xs
    formulaId (Quant Universal _) = ForallFormula
    formulaId (Quant Existential _) = ExistsFormula
    formulaId (Choice Universal f1 f2) = AndFormula f1 f2
    formulaId (Choice Existential f1 f2) = OrFormula f1 f2
    formulaId (Implies f1 f2) = ImpliesFormula f1 f2
    formulaId (Iff f1 f2) = IffFormula f1 f2
    formulaId (Not f) = NotFormula f
    formulaId Bottom = BottomFormula
    formulaId Top = TopFormula
    formulaId _ = error "halbdsc"

    cleanUpFormula :: Formula -> Formula
    cleanUpFormula (Paren f) = cleanUpFormula f
    cleanUpFormula (Quant p f) = Quant p $ cleanUpFormula f
    cleanUpFormula (Choice p f1 f2) = Choice p (cleanUpFormula f1) (cleanUpFormula f2)
    cleanUpFormula (Implies f1 f2) = Implies (cleanUpFormula f1) (cleanUpFormula f2)
    cleanUpFormula (Iff f1 f2) = Iff (cleanUpFormula f1) (cleanUpFormula f2)
    cleanUpFormula (Not f) = Not (cleanUpFormula f)
    cleanUpFormula (Relation sym xs) = Relation sym (map cleanUpTerm xs)
    cleanUpFormula f = f
    

    getTermDiff :: Var -> Term -> Term -> Maybe Term
    getTermDiff v v'@(Var _) t | Var v == v' && t /= v' = Just t
                               | otherwise = Nothing
    getTermDiff v (Sym sym1 xs1) (Sym sym2 xs2) 
        | sym1 == sym2 && length xs1 == length xs2 
            = foldl mplus mzero $ zipWith (getTermDiff v) xs1 xs2 
        | otherwise = Nothing
    getTermDiff _ _ _ = Nothing

    getFormulaDiff :: Formula -> Formula -> [Maybe Term]
    getFormulaDiff f01 f02 = init $ getDiff f01 f02
        where
            getDiff frfr@(Quant _ fr1) fr2 = case mt of
                    Nothing -> mt : getDiff fr1 fr2
                    Just t -> mt : getDiff (freeVar t frfr) fr2
                where
                    mt = if cDiff >=0 then innerDiff cDiff (unQuant cDiff fr1) fr2 else Nothing
                    innerDiff :: Int -> Formula -> Formula -> Maybe Term
                    innerDiff i (Relation sym1 xs1) (Relation sym2 xs2)
                        | sym1 == sym2 && length xs1 == length xs2 
                            = foldl mplus mzero $ zipWith (getTermDiff $ LocalVar i) xs1 xs2 
                        | otherwise = Nothing
                    innerDiff i (Quant p1 f1) (Quant p2 f2)
                        | p1 == p2 = innerDiff (i+1) f1 f2
                        | otherwise = Nothing
                    innerDiff i (Choice p1 f11 f12) (Choice p2 f21 f22) 
                        | p1 == p2 = innerDiff i f11 f21 `mplus` innerDiff i f12 f22
                        | otherwise = Nothing
                    innerDiff i (Implies f11 f12) (Implies f21 f22) = innerDiff i f11 f21 `mplus` innerDiff i f12 f22
                    innerDiff i (Iff f11 f12) (Iff f21 f22) = innerDiff i f11 f21 `mplus` innerDiff i f12 f22
                    innerDiff i (Not f1) (Not f2) = innerDiff i f1 f2
                    innerDiff _ _ _ = Nothing

                    unQuant :: Int -> Formula -> Formula
                    unQuant 0 f1 = f1
                    unQuant i (Quant _ f1) = unQuant (i-1) f1
                    unQuant _ _ = error "shgae"
                    quantQuant :: Formula -> Int
                    quantQuant (Quant _ f) = 1 + quantQuant f
                    quantQuant _ = 0
                    cDiff :: Int
                    cDiff = quantQuant frfr - quantQuant fr2-1
            getDiff _ _ = [Nothing]

    getRenameTerm :: MonadFail m => Term -> Term -> m [(String,String)]
    getRenameTerm (Var (FreeVar v)) (Var (FreeVar v')) = if v /= v' then return [(v,v')] else return []
    getRenameTerm (Var v1) (Var v2) = if v1 == v2 then return [] else fail ""
    getRenameTerm (Sym sym1 xs1) (Sym sym2 xs2) 
        | sym1 == sym2 && length xs1 == length xs2 = concat <$> zipWithM getRenameTerm xs1 xs2
        | otherwise = fail ""
    getRenameTerm _ _ = fail ""

    getRenameFormula :: MonadFail m => Formula -> Formula -> m [(String,String)]
    getRenameFormula (Relation sym1 xs1) (Relation sym2 xs2) = getRenameTerm (Sym sym1 xs1) (Sym sym2 xs2)
    getRenameFormula (Quant p1 f1) (Quant p2 f2) = 
        if p1 == p2 then getRenameFormula f1 f2 else fail ""
    getRenameFormula (Choice p1 f11 f12) (Choice p2 f21 f22) =
        if p1 == p2 then liftM2 mplus (getRenameFormula f11 f21) (getRenameFormula f12 f22) else fail ""
    getRenameFormula (Implies f11 f12) (Implies f21 f22) = 
        liftM2 mplus (getRenameFormula f11 f21) (getRenameFormula f12 f22)
    getRenameFormula (Iff f11 f12) (Iff f21 f22) = 
        liftM2 mplus (getRenameFormula f11 f21) (getRenameFormula f12 f22)
    getRenameFormula (Not f1) (Not f2) = getRenameFormula f1 f2
    getRenameFormula Top Top = return []
    getRenameFormula Bottom Bottom = return []
    getRenameFormula _ _ = fail ""

    matchFormulas :: MonadFail m => Formula -> Formula -> m (Map.Map String Term)
    matchFormulas f1 f2 = do
        mp <- getRenameFormula f1 f2 <&> map (second read) <&> Map.fromList
        True <- return (substManyInFormula mp f1 == f2)
        return mp

    allVars :: [String]
    allVars = map show [(toEnum 0::VarName)..]
    
    freshVars :: Formula -> [String]
    freshVars f = filter (not . flip freeInFormula f) allVars

    freshVarsInFormulas :: [Formula] -> [String]
    freshVarsInFormulas fs = filter (not . flip freeInFormulas fs) allVars
    
    instance Show Formula where
        showsPrec 0 fOrg s = innerShow (toEnum 0) fOrg ++ s
            where
                innerShow :: VarName -> Formula -> String
                innerShow _ (Relation sym xs) = showsPrec 6 (Sym sym xs) ""
                innerShow i f0@(Quant p _) =
                    let sym=case p of {Universal -> "A";Existential -> "E"} in
                    let name = show i in
                        if freeInFormula name fOrg
                           then innerShow (succ i) f0
                           else sym ++ name ++ "." ++ innerShow (succ i) (freeVar (Var . FreeVar $ name) f0)
                innerShow i (Choice p f1 f2) = 
                    let sym = case p of {Universal -> " & ";Existential -> " | "} in 
                    let show1 = case f1 of
                                    Not _ -> innerShow i f1
                                    Relation _ _ -> innerShow i f1
                                    _ -> "("++innerShow i f1++")"
                    in
                    let show2 = case f2 of
                                    Not _ -> innerShow i f2
                                    Relation _ _ -> innerShow i f2
                                    _ -> "("++innerShow i f2++")"
                    in concat [show1, sym, show2]
                innerShow i (Implies f1 f2) = 
                    let show1 = case f1 of
                                    Not _ -> innerShow i f1
                                    Relation _ _ -> innerShow i f1
                                    Choice{} -> innerShow i f1
                                    Iff{} -> innerShow i f1
                                    _ -> "("++innerShow i f1++")"
                    in
                    let show2 = case f2 of
                                    Quant _ _ -> "("++innerShow i f2++")"
                                    _ -> innerShow i f2
                    in concat [show1, " -> ", show2]
                innerShow i (Iff f1 f2) = 
                    let show1 = case f1 of
                                    Not _ -> innerShow i f1
                                    Relation _ _ -> innerShow i f1
                                    Choice{} -> innerShow i f1
                                    _ -> "("++innerShow i f1++")"
                    in
                    let show2 = case f2 of
                                    Quant _ _ -> "("++innerShow i f2++")"
                                    _ -> innerShow i f2
                    in concat [show1, " <=> ", show2]
                innerShow i (Not f1@(Not _)) = "~"++ innerShow i f1
                innerShow _ (Not Top) = "~T"
                innerShow _ (Not Bottom) = "~_"
                innerShow i (Not rel@(Relation sym xs)) 
                  | not (null sym) && all (`elem` operatorChars) sym && length xs == 2 = "~("++ innerShow i rel++")"
                  | otherwise = '~':innerShow i rel
                innerShow i (Not f1) = "~("++ innerShow i f1 ++")"
                innerShow _ Top = "T"
                innerShow _ Bottom = "_"
                innerShow _ (Paren _) = error "asdfuy"
        showsPrec _ fOrg s = "(" ++ shows fOrg ")" ++ s

    instance Read Formula where
        readsPrec 0 s = maybeToList $ do
                                (f,(LitA s1,_)) <- unNoInput $ runStateT readFormula $ toFormats s
                                guard $ f /= Relation "" []
                                return (f,s1) 
        readsPrec _ fstr = do (str, rest) <- parenSplit fstr
                              map (second (++rest)) $ reads str

    readFormula :: (MonadInput m,MonadPlus m,MonadState (AnyFormat,[AnyFormat]) m) => m Formula
    readFormula = get >>= put . cleanUpFormat >> readStateFormula <&> cleanUpFormula.($ (Relation "" [],Sym "" [])) 

    readStateFormula :: (MonadInput m,MonadPlus m,MonadState (AnyFormat,[AnyFormat]) m) => m ((Formula,Term) -> Formula)
    readStateFormula = do
        (xs0,xsF) <- get
        case xs0 of
            FormulaA -> do 
                frm <- input FormulaF
                f <- case xsF of
                        [] -> return fst
                        xf:xsf -> put (xf,xsf) >> readStateFormula
                return $ \_ -> f (Paren frm,Sym "" [])
            StringA -> input StringF >>= put . (,xsF) . LitA . filter (/=' ') >> readStateFormula
            TermA -> mzero
            FormatA -> inputFormat >>= put . (,xsF) >> readStateFormula
            x :^^: y -> put (x, y : xsF) >> readStateFormula 
            LitA xs -> case xs of
                [] -> case xsF of
                        [] -> return fst
                        (xf:xsf) -> put (xf,xsf) >> readStateFormula
                (',':_) -> mzero
                (')':xs1) -> put (LitA xs1,xsF) >> return fst
                (_:_) ->
                    let (sym,xs1) = span (`notElem` operatorChars++"(){},") $ filter (/=' ') xs in
                        let (sym1,xs11) = span (/=')') $ if null xs1 then [] else tail xs1 in
                            if null sym && not (null sym1) && all (`elem` operatorChars) sym1 && take 2 xs11 == ")("
                                then 
                                    do
                                        put (LitA $ drop 2 xs11,xsF)
                                        f <- readStateTerm 
                                        f1 <- readStateFormula
                                        let terms = getTerms $ cleanUpTerm $ Sym ('.':sym1) $ f $ Sym "" []
                                        return $ \_ -> f1 (Relation sym1 terms,Sym sym1 terms)
                                else let (symO,xsO) = span (`elem` operatorChars) xs
                                         isRel = (==Just '(') $ fst <$> List.uncons (dropWhile (`notElem` operatorChars++"(){},") xsO)
                                         theLeftTerm = leftRightTerm $ fst $ head $ reads xs
                                         rightTerm ff = case ff of 
                                            Relation symI terms | isRel  -> Sym symI terms
                                            _ -> theLeftTerm
                                         relation = do
                                            put (LitA xsO,xsF)
                                            f <- readStateFormula
                                            return $ case f mf of
                                                Implies f1 f2 ->
                                                    case f1 of
                                                        Choice p f11 f12 ->
                                                            \(_,t0) -> Implies (Choice p (Relation symO [t0,rightTerm f11]) f12) f2
                                                        _ -> \(_,t0) -> Implies (Relation symO [t0,rightTerm f1]) f2
                                                Choice p f1 f2 ->
                                                    \(_,t0) -> Choice p (Relation symO [t0,rightTerm f1]) f2
                                                f0 ->
                                                    \(_,t0) -> Relation symO [t0,rightTerm f0]
                                        in
                                case symO of
                                    ('-':'>':sym2) -> do
                                        put (LitA $ sym2++xsO,xsF)
                                        f <- readStateFormula
                                        return $ \(f1,_) -> Implies f1 $ f mf
                                    ('<':'=':'>':sym2) -> do
                                        put (LitA $ sym2++xsO,xsF)
                                        f <- readStateFormula
                                        return $ case f mf of
                                            Implies f1 f2 ->
                                                \(f0,_) -> Implies (Iff f0 f1) f2
                                            f1 ->
                                                \(f0,_) -> Iff f0 f1
                                    ('|':sym2) -> do
                                        put (LitA $ sym2++xsO,xsF)
                                        f <- readStateFormula
                                        return $ case f mf of
                                            Implies f1 f2 ->
                                                case f1 of
                                                    Iff f11 f22 ->
                                                         \(f0,_) -> Implies (Iff (Choice Existential f0 f11) f22) f2
                                                    _ -> \(f0,_) -> Implies (Choice Existential f0 f1) f2
                                            Iff f1 f2 -> \(f0,_) -> Iff (Choice Existential f0 f1) f2
                                            f1 ->
                                                \(f0,_) -> Choice Existential f0 f1
                                    ('&':sym2) -> do
                                        put (LitA $ sym2++xsO,xsF)
                                        f <- readStateFormula
                                        return $ case f mf of
                                            Implies f1 f2 ->
                                                case f1 of
                                                    Iff f11 f22 ->
                                                         \(f0,_) -> Implies (Iff (Choice Universal f0 f11) f22) f2
                                                    _ -> \(f0,_) -> Implies (Choice Universal f0 f1) f2
                                            Iff f1 f2 -> \(f0,_) -> Iff (Choice Universal f0 f1) f2
                                            f1 ->
                                                \(f0,_) -> Choice Universal f0 f1
                                    ('~':sym2) ->
                                            let negations = do
                                                    put (LitA $ sym2++xsO,xsF)
                                                    f <- readStateFormula
                                                    return $ case f mf of
                                                        Implies f1 f2 ->
                                                            case f1 of
                                                                Iff f11 f21 ->
                                                                    case f11 of
                                                                        Choice p f12 f22 ->
                                                                            \case
                                                                                (Relation "" [],_) -> Implies (Iff (Choice p (Not f12) f22) f21) f2
                                                                                (_,t0) -> Implies (Iff (Choice p (Relation symO [t0,rightTerm f12]) f22) f21) f2
                                                                        _ -> 
                                                                            \case 
                                                                                (Relation "" [],_) -> Implies (Iff (Not f11) f21) f2
                                                                                (_,t0) -> Implies (Iff (Relation symO [t0,rightTerm f11]) f21) f2
                                                                        
                                                                Choice p f11 f12 ->
                                                                    \case 
                                                                        (Relation "" [],_) -> Implies (Choice p (Not f11) f12) f2
                                                                        (_,t0) -> Implies (Choice p (Relation symO [t0,rightTerm f11]) f12) f2
                                                                _ ->
                                                                    \case
                                                                        (Relation "" [],_) -> Implies (Not f1) f2
                                                                        (_,t0) -> Implies (Relation symO [t0,rightTerm f1]) f2
                                                        Iff f1 f2 ->
                                                            case f1 of
                                                                Choice p f12 f22 ->
                                                                    \case
                                                                        (Relation "" [],_) -> Iff (Choice p (Not f12) f22) f2
                                                                        (_,t0) -> Iff (Choice p (Relation symO [t0,rightTerm f12]) f22) f2
                                                                _ -> 
                                                                    \case 
                                                                        (Relation "" [],_) -> Iff (Not f1) f2
                                                                        (_,t0) -> Iff (Relation symO [t0,rightTerm f1]) f2

                                                        Choice p f1 f2 ->
                                                            \case
                                                                (Relation "" [],_) -> Choice p (Not f1) f2
                                                                (_,t0) -> Choice p (Relation symO [t0,rightTerm f1]) f2
                                                        f0 ->
                                                            \case
                                                                (Relation "" [],_) -> Not f0
                                                                (_,t0) -> Relation symO [t0,rightTerm f0]
                                                in 
                                        case sym2++xsO of
                                            ('(':xsO1) -> 
                                                let (sym11,xs111) = span (/=')') xsO1 in
                                                if take 2 xs111 == ")(" && not (null sym11) && all (`elem` operatorChars) sym11
                                                    then negations
                                                    else do
                                                        let formulaRead = do
                                                                put (LitA xsO1,xsF)
                                                                f <- readStateFormula
                                                                f1 <- readStateFormula
                                                                let f0 = f mf 
                                                                return $ \case
                                                                    (Relation "" [],t0) -> 
                                                                            f1 (Not (f mf),Sym symO [t0,rightTerm f0])
                                                                    (_,t0) -> let term = Sym symO [t0,theLeftTerm] in f1 (Relation symO [t0,theLeftTerm],term) 
                                                        let termRead = do
                                                                put (LitA xsO1,xsF)
                                                                f <- readStateTerm
                                                                f1 <- readStateFormula
                                                                return $ \(_,t0) -> let terms = [t0,Sym "" $ f $ Sym "" []] in f1 (Relation symO terms,Sym symO terms) 
                                                        formulaRead `mplus` termRead
                                            
                                            _ -> if all (=='~') sym2 then negations else relation
                                    (_:_) -> 
                                        case xsO of 
                                            ('(':xsO1) -> 
                                                let (sym11,xs111) = span (/=')') xsO1 in
                                                if take 2 xs111 == ")(" && not (null sym11) && all (`elem` operatorChars) sym11
                                                    then relation
                                                    else do
                                                        put (LitA xsO1,xsF)
                                                        f <- readStateTerm
                                                        f1 <- readStateFormula
                                                        let term2 = Sym "" $ f $ Sym "" []
                                                        return $ \(_,t0) -> let terms = [t0,term2] in f1 (Relation symO terms,Sym symO terms)
                                            _ -> relation                                            
                                    [] ->
                                        let mf1 = (Relation "" [],Sym "" []) in
                                        if null xs1 || head xs1 `notElem` "(.{" || (head xs1 == '.' && take 1 sym `notElem` ["A","E"]) 
                                            then do 
                                                put (LitA xs1,xsF)
                                                f <- readStateFormula
                                                return $ \_ -> f $ (,read sym) $ case sym of
                                                    "_" -> Bottom
                                                    "T" -> Top
                                                    _   -> Relation sym [] 
                                            else if head xs1 == '.' && take 1 sym `elem` ["A","E"] 
                                            then do
                                                put (LitA $ tail xs1,xsF)
                                                f1 <- readStateFormula
                                                let p = case head sym of {'A' -> Universal;'E' -> Existential;_ -> error "adfs"}
                                                return $ \_ -> bindVar p (tail sym) $ f1 mf1
                                            else if head xs1 == '!' && take 1 sym == "E"
                                            then do
                                                put (LitA $ tail xs1,xsF)
                                                f1 <- readStateFormula <&> ($ mf1)
                                                let x = tail sym
                                                let vars = freshVars f1;y = head vars;z = head $ tail vars
                                                let p = existsFormula x f1
                                                let p2 = forallFormula y $ forallFormula z $ Implies (substInFormula x (read y) f1) $ Implies (substInFormula x (read z) f1) (Relation "=" [read y,read z])
                                                return $ \_ -> Choice Universal p p2
                                            else
                                                let rel = do
                                                        put (LitA $ tail xs1,xsF)
                                                        f <- readStateTerm
                                                        f1 <- readStateFormula
                                                        let terms = getTerms $ cleanUpTerm $ Sym ('0':sym) $ f $ Sym "" []
                                                        return (\_ -> f1 (Relation sym terms,Sym sym terms))
                                                    subst = do
                                                        put (LitA $ tail xs1,xsF)
                                                        f <- readStateTerm
                                                        f1 <- readStateFormula
                                                        let substMap = Map.fromList $ mapMaybe (\case (Sym _  [Var (FreeVar s),t]) -> Just (s,t); _ -> Nothing) $ f $ Sym "" []
                                                        return $ if null sym 
                                                            then f1 . bimap (substManyInFormula substMap) (substManyInTerm substMap)
                                                            else \_ -> f1 mf1
                                                in
                                                if null sym 
                                                    then 
                                                        let formulaRead = do
                                                                ft <- put (LitA $ tail xs1,xsF) >> readStateTerm `mplus` return (const [])
                                                                put (LitA $ tail xs1,xsF)
                                                                f <- readStateFormula
                                                                f1 <- readStateFormula
                                                                return $ \_ -> f1 (Paren $ f mf1,Sym "" $ ft $ Sym "" [])
                                                        in if head xs1 == '{'
                                                            then subst
                                                            else formulaRead `mplus` rel
                                                    else if head xs1 == '{'
                                                            then subst
                                                            else rel
        where
            mf :: (Formula,Term)
            mf = (Relation "" [],Sym "" [])

            leftRightTerm :: Term -> Term
            leftRightTerm (Sym _ [Sym "" [],Sym ('-':'>':_) [x,_]]) = x
            leftRightTerm (Sym _ [Sym "" [],Sym ('<':'=':'>':_) [x,_]]) = x
            leftRightTerm (Sym _ [Sym "" [],Sym ('|':_) [x,_]]) = x
            leftRightTerm (Sym _ [Sym "" [],Sym ('&':_) [x,_]]) = x
            leftRightTerm (Sym _ [Sym "" [],x]) = x
            leftRightTerm t = t



    freeInTerm :: String -> Term -> Bool
    freeInTerm s (Var (FreeVar v)) = s==v
    freeInTerm s (Sym _ xs) = any (freeInTerm s) xs
    freeInTerm _ _ = False

    freeInFormula :: String -> Formula -> Bool
    freeInFormula s (Relation _ xs) = any (freeInTerm s) xs
    freeInFormula s (Quant _ f) = freeInFormula s f
    freeInFormula s (Choice _ f1 f2) = freeInFormula s f1 || freeInFormula s f2
    freeInFormula s (Implies f1 f2) = freeInFormula s f1 || freeInFormula s f2
    freeInFormula s (Iff f1 f2) = freeInFormula s f1 || freeInFormula s f2
    freeInFormula s (Not f) = freeInFormula s f
    freeInFormula s (Paren f) = freeInFormula s f
    freeInFormula _ _ = False

    freeInFormulas :: Foldable f => String -> f Formula -> Bool
    freeInFormulas var = any (freeInFormula var)

    substInTerm :: String -> Term -> Term -> Term
    substInTerm var sub v@(Var _) | v==makeVar var = sub
                                  | otherwise = v
    substInTerm var sub (Sym sym xs) = Sym sym (map (substInTerm var sub) xs)

    substVarInTerm :: Var -> Term -> Term -> Term
    substVarInTerm var sub (Var v) | v==var = sub
                                   | otherwise = Var v
    substVarInTerm var sub (Sym sym xs) = Sym sym (map (substVarInTerm var sub) xs)

    substInFormula :: String -> Term -> Formula -> Formula
    substInFormula var sub = substInInnerFormula
        where
            substInInnerFormula :: Formula -> Formula
            substInInnerFormula (Relation sym xs) = Relation sym $ getTerms $ substVarInTerm (FreeVar var) sub (Sym sym xs)
            substInInnerFormula (Quant p f) = Quant p (substInInnerFormula f)
            substInInnerFormula (Choice p f1 f2) = Choice p (substInInnerFormula f1) (substInInnerFormula f2)
            substInInnerFormula (Implies f1 f2) = Implies (substInInnerFormula f1) (substInInnerFormula f2)
            substInInnerFormula (Iff f1 f2) = Iff (substInInnerFormula f1) (substInInnerFormula f2)
            substInInnerFormula (Not f) = Not (substInInnerFormula f)
            substInInnerFormula (Paren f) = Paren (substInInnerFormula f)
            substInInnerFormula f = f
            
    substManyInTerm :: Map.Map String Term -> Term -> Term
    substManyInTerm mp var@(Var (FreeVar v)) = fromMaybe var (Map.lookup v mp)
    substManyInTerm mp (Sym sym xs) = Sym sym (map (substManyInTerm mp) xs)
    substManyInTerm _ v = v

    substManyInFormula :: Map.Map String Term -> Formula -> Formula
    substManyInFormula mp = substInInnerFormula
        where
            substInInnerFormula :: Formula -> Formula
            substInInnerFormula (Relation sym xs) = Relation sym (map (substManyInTerm mp) xs)
            substInInnerFormula (Quant p f) = Quant p (substInInnerFormula f)
            substInInnerFormula (Choice p f1 f2) = Choice p (substInInnerFormula f1) (substInInnerFormula f2)
            substInInnerFormula (Implies f1 f2) = Implies (substInInnerFormula f1) (substInInnerFormula f2)
            substInInnerFormula (Iff f1 f2) = Iff (substInInnerFormula f1) (substInInnerFormula f2)
            substInInnerFormula (Not f) = Not (substInInnerFormula f)
            substInInnerFormula (Paren f) = Paren (substInInnerFormula f)
            substInInnerFormula f = f

    bindVar :: Player -> String -> Formula -> Formula
    bindVar p var  = Quant p . bindInner 0  
        where
            bindInner :: Int -> Formula -> Formula
            bindInner i (Relation sym xs) = Relation sym $ getTerms $ substVarInTerm (FreeVar var) (Var $ LocalVar i) (Sym sym xs) 
            bindInner i (Quant p1 f) = Quant p1 (bindInner (i+1) f)
            bindInner i (Choice p1 f1 f2) = Choice p1 (bindInner i f1) (bindInner i f2)
            bindInner i (Implies f1 f2) = Implies (bindInner i f1) (bindInner i f2)
            bindInner i (Iff f1 f2) = Iff (bindInner i f1) (bindInner i f2)
            bindInner i (Not f) = Not (bindInner i f)
            bindInner i (Paren f) = Paren (bindInner i f)
            bindInner _ f = f

    freeVar :: Term -> Formula -> Formula
    freeVar term (Quant _ f0) = freeInner 0 f0
        where
            freeInner :: Int -> Formula -> Formula
            freeInner i (Relation sym xs) = Relation sym $ getTerms $ substVarInTerm (LocalVar i) term $ Sym sym xs
            freeInner i (Quant p f) = Quant p $ freeInner (i+1) f
            freeInner i (Choice p f1 f2) = Choice p (freeInner i f1) (freeInner i f2)
            freeInner i (Implies f1 f2) = Implies (freeInner i f1) (freeInner i f2)
            freeInner i (Iff f1 f2) = Iff (freeInner i f1) (freeInner i f2)
            freeInner i (Not f) = Not (freeInner i f)
            freeInner i (Paren f) = Paren (freeInner i f)
            freeInner _ f = f
    freeVar _ f = f

    forallFormula :: String -> Formula -> Formula
    forallFormula = bindVar Universal 
    existsFormula :: String -> Formula -> Formula
    existsFormula = bindVar Existential
    andFormula :: Formula -> Formula -> Formula
    andFormula = Choice Universal
    orFormula :: Formula -> Formula -> Formula
    orFormula = Choice Existential
    impliesFormula :: Formula -> Formula -> Formula
    impliesFormula = Implies
    iffFormula :: Formula -> Formula -> Formula
    iffFormula = Iff
    notFormula :: Formula -> Formula
    notFormula = Not
    topFormula :: Formula
    topFormula = Top
    bottomFormula :: Formula
    bottomFormula = Bottom

    makeRelation :: String -> [Term] -> Formula
    makeRelation sym = if all (`notElem` ",()") sym && (all (`elem` operatorChars) sym || all (`notElem` operatorChars) sym) then Relation sym else error "Invalid relation symbol" 
    makeSym :: String -> [Term] -> Term
    makeSym sym = if all (`notElem` ",()") sym && (all (`elem` operatorChars) sym || all (`notElem` operatorChars) sym) then Sym sym else error "Invalid term symbol" 
    makeVar :: String -> Term
    makeVar sym = case read sym of Var var -> Var var;_ -> error "not a variable"

    getTerms :: Term -> [Term]
    getTerms (Sym _ xs) = xs
    getTerms _ = error "not a sym"

    data Format a b where
        Lit :: String -> Format a a
        StringF :: Format (String -> a) a
        TermF :: Format (Term -> a) a
        FormulaF :: forall a. Format (Formula -> a) a
        FormatF :: Format (Format a b -> a) b
        (:^:) :: Format a b -> Format b c -> Format a c
    infixr 9 :^:

    substFormat :: String -> Format (Formula -> String -> a) a
    substFormat t = FormulaF :^: Lit "{" :^: StringF :^: Lit ("/" ++ t ++"}")

    data AnyFormat = 
        LitA String | FormulaA | TermA | StringA | FormatA | AnyFormat :^^: AnyFormat deriving(Show)
    infixr 9 :^^:
    
    instance Read AnyFormat where
        readPrec = parens $
            (do
                Ident "f" <- lexP
                f <- readPrec
                return $ FormulaA :^^: f
                ) +++
            (do
                Ident "t" <- lexP
                f <- readPrec
                return $ TermA :^^: f) +++
            (do
                Ident "s" <- lexP
                f <- readPrec
                return $ StringA :^^: f)  +++
            (do
                Ident "F" <- lexP
                f <- readPrec
                return $ FormatA :^^: f) +++
            (do
                s@(_:_) <- parens readPrec
                f1 <- readPrec
                return $ LitA s :^^: f1) +++ return (LitA "")

    data AnyFill =
          WithFormula Formula
        | WithTerm Term 
        | WithString String
        | WithFormat AnyFormat
        deriving Show
    class Monad m => MonadInput m where
        input :: Format (a -> b) b -> m a
        inputFormat :: m AnyFormat
    newtype NoInput m a = NoInput{unNoInput :: m a}
        deriving (Functor, Applicative, Alternative, Monad, MonadPlus, MonadIO)
    instance MonadTrans NoInput where
        lift = NoInput 
    
    newtype ListState m a = LState (StateT [AnyFill] m a)
        deriving (Functor, Applicative, Monad, Alternative, MonadPlus, MonadFail, MonadTrans, MonadIO)
    evalList :: Monad m => ListState m a -> [AnyFill] -> m a
    evalList (LState s) = evalStateT s



    instance MonadFail m => MonadInput (ListState m) where
        input format = LState $ do
            (x : xs) <- get
            case (x,format) of
              (WithFormula f,FormulaF) -> put xs >> return f
              (WithTerm t,TermF) -> put xs >> return t
              (WithString s,StringF) -> put xs >> return s
              _ -> fail $ show (x,anyFormat format)
        inputFormat = LState $ do
            (x : xs) <- get
            case x of
                WithFormat f -> put xs >> return f
                _ -> fail "Invalid input"

    instance (MonadInput m) => MonadInput (StateT s m) where
        input = lift . input
        inputFormat = lift inputFormat
    instance (MonadInput m,Monoid s) => MonadInput (AccumT s m) where
        input = lift . input
        inputFormat = lift inputFormat
    instance (MonadInput m) => MonadInput (ReaderT s m) where
        input = lift . input
        inputFormat = lift inputFormat

    instance Monad m => MonadInput (NoInput m) where
        input = error "Invalid input"
        inputFormat = error "Invalid input"

    ksprintf :: Format a b -> ([AnyFill] -> b) -> a
    ksprintf format cont = case format of
        FormulaF -> cont . (:[]) . WithFormula
        TermF -> cont . (:[]) . WithTerm
        StringF -> cont . (:[]) . WithString
        FormatF ->  \f -> ksprintf f (cont . (WithFormat (anyFormat f):))
        x :^: y -> ksprintf x (\xs1 -> ksprintf y (\xs2 -> cont $ xs1++xs2))
        Lit _ -> cont []

    anyFormat :: Format a b -> AnyFormat
    anyFormat (Lit s) = LitA s
    anyFormat FormulaF = FormulaA
    anyFormat TermF = TermA
    anyFormat StringF = StringA
    anyFormat FormatF = FormatA
    anyFormat (x :^: y) = anyFormat x :^^: anyFormat y

    evalFormat :: Monad m => AnyFormat -> StateT (AnyFormat,[AnyFormat]) m a -> m a
    evalFormat format readM = evalStateT readM (format,[])

    sprintfF :: Format a Formula -> a
    sprintfF format = ksprintf format $ \xs -> fromJust (evalList (evalFormat (anyFormat format) readFormula) xs)

    sprintfT :: Format a Term -> a
    sprintfT format = ksprintf format $ \xs -> fromJust (evalList ( evalFormat (anyFormat format) readTerm) xs)


