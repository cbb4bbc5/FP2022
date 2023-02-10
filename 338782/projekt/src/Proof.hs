{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
module Proof(Proof(..),proof,qed,goal,next,
                intro,apply,applyThm,applyAxiom,applyAss,split,trivial,
                addFormula,chooseFormula,getExample,getForall,getForalls,
                qedM,
                applyM,applyThmM,applyAxiomM,applyAssM,trivialM,
                addFormulaM,chooseFormulaM,getM,getsM) where
    import Formula
    import Axiom
    import Logic
    import Data.Maybe 
    import qualified Data.Map as Map
    import Control.Monad
    
    data Proof = Complete Theorem | Incomplete ProofTree ProofContext
    data ProofTree = 
          Proven Theorem
        | Goal (Map.Map String Formula) Formula
        | ImpI Formula ProofTree
        | ImpE ProofTree ProofTree
        | IffI ProofTree ProofTree
        | IffE ProofTree ProofTree
        | BotE Formula ProofTree
        | AndI ProofTree ProofTree
        | AndE1 ProofTree
        | AndE2 ProofTree
        | OrI1 ProofTree Formula
        | OrI2 Formula ProofTree
        | OrE ProofTree ProofTree ProofTree
        | ExistsI ProofTree Formula
        | ExistsE String ProofTree ProofTree
        | ForallI String ProofTree
        | ForallE Term ProofTree 
        deriving Eq
    data ProofContext =
          Top 
        | CImpI Formula ProofContext
        | CImpEF ProofTree ProofContext
        | CImpEI ProofContext ProofTree
        | CIffI1 ProofContext ProofTree
        | CIffI2 ProofTree ProofContext
        | CIffEF ProofTree ProofContext
        | CIffEI ProofContext ProofTree
        | CBotE Formula ProofContext
        | CAndI1 ProofContext ProofTree
        | CAndI2 ProofTree ProofContext
        | CAndE1 ProofContext
        | CAndE2 ProofContext
        | COrI1 ProofContext Formula
        | COrI2 Formula ProofContext
        | COrE0 ProofContext ProofTree ProofTree
        | COrE1 ProofTree ProofContext ProofTree
        | COrE2 ProofTree ProofTree ProofContext
        | CExistsI ProofContext Formula
        | CExistsEE String ProofContext ProofTree
        | CExistsEF String ProofTree ProofContext
        | CForallI String ProofContext
        | CForallE Term ProofContext

    instance Show Proof where
        show pr = case goal pr of
            Nothing -> "No more subgoals"
            Just (xs,f) -> innerShow xs ++ replicate 12 '=' ++ "\n" ++ show f
          where
            innerShow :: [(String,Formula)] -> String
            innerShow [] = ""
            innerShow ((s,f):xs) = s ++ ": " ++ show f ++ "\n" ++ innerShow xs

    proof :: [(String,Formula)] -> Formula -> Proof
    proof xs f = Incomplete (Goal (Map.fromList xs) f) Top

    qed :: Proof -> Maybe Theorem
    qed (Complete thm) = Just thm
    qed _ = Nothing 

    goal :: Proof -> Maybe ([(String, Formula)],Formula)
    goal (Incomplete (Goal xs f) _) = Just (Map.toList xs,f)
    goal _ = Nothing


    upright :: (ProofTree,ProofContext) -> (ProofTree,ProofContext)
    upright (tree,Top) = (tree,Top)
    upright (tree,CImpI f cont) = upright (ImpI f tree,cont)
    upright (tree,CBotE f cont) = upright (BotE f tree,cont)
    upright (tree,CImpEF imp cont) = upright (ImpE imp tree,cont)
    upright (tree,CImpEI cont thm@(Proven _)) = upright (ImpE tree thm,cont)
    upright (tree,CImpEI cont f) = (f,CImpEF tree cont)
    upright (tree,CIffI1 cont f) = (f,CIffI2 tree cont)
    upright (tree,CIffI2 f cont) = upright (IffI f tree,cont)
    upright (tree,CIffEF imp cont) = upright (IffE imp tree,cont)
    upright (tree,CIffEI cont thm@(Proven _)) = upright (IffE tree thm,cont)
    upright (tree,CIffEI cont f) = (f,CIffEF tree cont)
    upright (tree,CAndI1 cont thm@(Proven _)) = upright (AndI tree thm,cont)
    upright (tree,CAndI1 cont f) = (f,CAndI2 tree cont)
    upright (tree,CAndI2 f cont) = upright (AndI f tree,cont)
    upright (tree,CAndE1 cont) = upright (AndE1 tree,cont)
    upright (tree,CAndE2 cont) = upright (AndE2 tree,cont)
    upright (tree,COrI1 cont f) = upright (OrI1 tree f,cont)
    upright (tree,COrI2 f cont) = upright (OrI2 f tree,cont)
    upright (tree,COrE0 cont thm1@(Proven _) thm2@(Proven _)) = upright (OrE tree thm1 thm2,cont)
    upright (tree,COrE0 cont thm1@(Proven _) f) = (f,COrE2 tree thm1 cont)
    upright (tree,COrE0 cont f1 f2) = (f1,COrE1 tree cont f2)
    upright (tree,COrE1 f0 cont thm@(Proven _)) = upright (OrE f0 tree thm,cont)
    upright (tree,COrE1 f0 cont f2) = (f2,COrE2 f0 tree cont)
    upright (tree,COrE2 f0 f1 cont) = (OrE f0 f1 tree,cont)
    upright (tree,CExistsI cont f) = upright (ExistsI tree f,cont)
    upright (tree,CExistsEF x exi cont) = upright (ExistsE x exi tree,cont)
    upright (tree,CExistsEE x cont thm@(Proven _)) = upright (ExistsE x tree thm,cont)
    upright (tree,CExistsEE x cont f) = (f,CExistsEF x tree cont)
    upright (tree,CForallI var cont) = upright (ForallI var tree,cont)
    upright (tree,CForallE t cont) = upright (ForallE t tree,cont)

    downleft :: (ProofTree,ProofContext) -> (ProofTree,ProofContext)
    downleft (ImpI f tree,cont) = downleft (tree,CImpI f cont)
    downleft (BotE f tree,cont) = downleft (tree,CBotE f cont)
    downleft (ImpE thm@(Proven _) tree,cont) = downleft (tree,CImpEF thm cont)
    downleft (ImpE tree f,cont) = downleft (tree,CImpEI cont f)
    downleft (IffI thm@(Proven _) tree,cont) = downleft (tree,CIffI2 thm cont)
    downleft (IffI tree f,cont) = downleft (tree,CIffI1 cont f)
    downleft (IffE thm@(Proven _) tree,cont) = downleft (tree,CIffEF thm cont)
    downleft (IffE tree f,cont) = downleft (tree,CIffEI cont f)
    downleft (AndI thm@(Proven _) tree,cont) = downleft (tree,CAndI2 thm cont)
    downleft (AndI tree f,cont) = downleft (tree,CAndI1 cont f)
    downleft (AndE1 tree,cont) = downleft (tree,CAndE1 cont)
    downleft (AndE2 tree,cont) = downleft (tree,CAndE2 cont)
    downleft (OrI1 tree f,cont) = downleft (tree,COrI1 cont f)
    downleft (OrI2 f tree,cont) = downleft (tree,COrI2 f cont)
    downleft (OrE thm0@(Proven _) thm1@(Proven _) tree,cont) = downleft (tree,COrE2 thm0 thm1 cont)
    downleft (OrE thm0@(Proven _) tree f2,cont) = downleft (tree,COrE1 thm0 cont f2)
    downleft (OrE tree f1 f2,cont) = downleft (tree,COrE0 cont f1 f2)
    downleft (ExistsI tree f,cont) = downleft (tree,CExistsI cont f)
    downleft (ExistsE x thm@(Proven _) tree,cont) = downleft (tree,CExistsEF x thm cont)
    downleft (ExistsE x tree f,cont) = downleft (tree,CExistsEE x cont f)
    downleft (ForallI str tree,cont) = downleft (tree,CForallI str cont)
    downleft (ForallE t tree,cont) = downleft (tree,CForallE t cont)
    downleft x = x

    pairToProof :: (ProofTree,ProofContext) -> Proof
    pairToProof = uncurry Incomplete

    next :: Proof -> Proof
    next pf@(Complete _) = pf
    next (Incomplete curTree curCont) = complete curTree curCont
        where
            complete :: ProofTree -> ProofContext ->  Proof
            complete tree@(Proven thm) cont0 =
                case cont0 of
                    Top -> Complete thm
                    CImpI f cont -> complete (Proven (impliesI thm f)) cont
                    CBotE f cont -> complete (Proven (bottomE f thm)) cont
                    CImpEF (Proven thmI) cont -> complete (Proven (impliesE thmI thm)) cont
                    CImpEI cont (Proven thmF) -> complete (Proven (impliesE thm thmF)) cont
                    CIffEF (Proven thmI) cont -> complete (Proven (iffE thmI thm)) cont
                    CIffEI cont (Proven thmF) -> complete (Proven (iffE thm thmF)) cont
                    CIffI1 cont (Proven thm2) -> complete (Proven (iffI thm thm2)) cont
                    CIffI2 (Proven thm1) cont -> complete (Proven (iffI thm1 thm)) cont
                    CAndI1 cont (Proven thm2) -> complete (Proven (andI thm thm2)) cont
                    CAndI2 (Proven thm1) cont -> complete (Proven (andI thm1 thm)) cont
                    CAndE1 cont -> complete (Proven (andLeftE thm)) cont
                    CAndE2 cont -> complete (Proven (andRightE thm)) cont
                    COrI1 cont f -> complete (Proven (orRightI thm f)) cont
                    COrI2 f cont -> complete (Proven (orLeftI f thm)) cont
                    COrE0 cont (Proven thm1) (Proven thm2) -> complete (Proven (orE thm thm1 thm2)) cont
                    COrE1 (Proven thm0) cont (Proven thm2) -> complete (Proven (orE thm0 thm thm2)) cont
                    COrE2 (Proven thm0) (Proven thm1) cont -> complete (Proven (orE thm0 thm1 thm)) cont
                    CExistsI cont f -> complete (Proven (existsI thm f)) cont
                    CExistsEE x cont (Proven thm1) -> complete (Proven (existsE x thm thm1)) cont
                    CExistsEF x (Proven thm1) cont -> complete (Proven (existsE x thm1 thm)) cont
                    CForallI str cont -> complete (Proven (forallI str thm)) cont
                    CForallE term cont -> complete (Proven (forallE term thm)) cont
                    _ -> move (tree,cont0)
            complete tree cont = move (tree,cont)

            move :: (ProofTree,ProofContext) -> Proof
            move = pairToProof . downleft . upright

    intro :: String -> Proof -> Proof
    intro name (Incomplete (Goal as f) cont) | ImpliesFormula f1 f2 <- formulaId f = Incomplete (Goal (Map.insert name f1 as) f2) (CImpI f1 cont)
                                             | NotFormula f1 <- formulaId f = Incomplete (Goal (Map.insert name f1 as) bottomFormula) (CImpI f1 cont)
                                             | otherwise = error "not an implies"
    intro _ p = p

    implist :: Formula -> [Formula] -> Formula -> (Formula,[Formula],Maybe (Map.Map String Term))
    implist f0 acc f 
        | Just mp <- matchFormulas f f0 = (substManyInFormula mp f,map (substManyInFormula mp) acc,Just mp)
        | ImpliesFormula f1 f2 <- formulaId f = implist f0 (f1 : acc) f2
        | otherwise = (f,acc,Nothing)

    expandContext :: Map.Map String Formula -> Formula -> ProofContext -> [Formula] -> Proof
    expandContext as f0 cont [] = Incomplete (Goal as f0) cont
    expandContext as f0 cont (x:xs) = expandContext as f0 (CImpEI cont (Goal as x)) xs

    findAnd :: Formula -> Formula -> ProofContext -> Maybe ProofContext -- () -> (& & &) ->
    findAnd f0 f cont 
        | f == f0 = Just cont
        | AndFormula f1 f2 <- formulaId f = sides f1 f2
        | otherwise = Nothing
        where 
            sides f1 f2
                | Just cont1 <- findAnd f0 f1 cont = Just $ CAndE1 cont1
                | Just cont2 <- findAnd f0 f2 cont = Just $ CAndE2 cont2
                | otherwise = Nothing

    findOr :: Formula -> Formula -> ProofContext -> Maybe ProofContext -- (| | |) -> () ->
    findOr f0 f cont 
        | f == f0 = Just cont
        | OrFormula f1 f2 <- formulaId f0 = 
            maybe (findOr f2 f cont >>= (Just . COrI2 f1)) 
                (\cont1 -> Just $ COrI1 cont1 f2) (findOr f1 f cont)
        | otherwise = Nothing
    
    findForall :: [Term] -> Formula -> Formula -> ProofContext -> Maybe ProofContext
    findForall [] f f0 cont | f == f0 = Just cont
                            | otherwise = Nothing
    findForall (t:ts) f f0 cont 
        | ForallFormula <- formulaId f = fmap (CForallE t) (findForall ts (freeVar t f) f0 cont)
        | otherwise = findForall [] f f0 cont

    apply :: Formula -> Proof -> Proof 
    apply _ thm@(Complete _) = thm
    apply f (Incomplete (Goal as f0) cont) 
      | f0 == ff = expandContext as f' cont flist
      | Just contAnd <- findAnd f0 ff cont = Incomplete (Goal as ff) contAnd
      | Just contOr <- findOr f0 ff cont = Incomplete (Goal as ff) contOr
      | otherwise =
        case formulaId ff of
            BottomFormula -> expandContext as f' (CBotE f0 cont) flist
            NotFormula f1 -> expandContext as f' (CBotE f0 cont) (f1 : flist)
            ExistsFormula -> expandContext as f' (CExistsEE var cont $ Goal (Map.insert var (freeVar (makeVar var) ff) as) f0 ) flist
            OrFormula f1 f2 -> expandContext as f' 
                (COrE0 cont (Goal (Map.insert var (freeVar (makeVar var) f1) as) f0) (Goal (Map.insert var (freeVar (makeVar var) f2) as) f0)) flist
            ForallFormula -> let terms = map (\case Just t -> t; Nothing -> makeSym "" []) $ getFormulaDiff ff f0 in 
                case findForall terms ff f0 cont of 
                    Just cont1 -> expandContext as f' cont1 flist
                    Nothing -> error "mismatched_formula_"
            IffFormula f1 f2 | f0 == f1 -> expandContext as f' (CIffEI cont (Goal as f2)) flist
            IffFormula f1 f2 | f0 == f2 -> expandContext as f' (CIffEI cont (Goal as f1)) flist
            _ -> error "mismatched formula"
        where
            var =head $ filter (`notElem` keys) $ freshVarsInFormulas (f0 : ff : elems)
            elems = Map.elems as
            keys = Map.keys as
            (ff,flist,mp) = implist f0 [] f
            f' = maybe f (`substManyInFormula` f) mp

    apply _ _ = error "Not a goal"

    applyThm :: Theorem -> Proof -> Proof
    applyThm thm pr 
        | (Incomplete (Goal _ ff) cont) <- apply (consequence thm) pr, Just mp <- matchFormulas (consequence thm) ff =
            next $ Incomplete (Proven $ substManyInTheorem thm mp) cont
        | otherwise = error "mismatch formula"

    applyAxiom :: Axiom a => a -> Proof -> Proof
    applyAxiom = applyThm . fromAxiom

    applyAss :: String -> Proof -> Proof
    applyAss str pr@(Incomplete (Goal as _) _) 
        | (Incomplete (Goal _ ff) cont) <- apply (fromJust f1) pr, f1 == Just ff = next (Incomplete (Proven $ aX ff) cont)
        | otherwise = error "mismatch formula"
            where f1 = Map.lookup str as
    applyAss _ pr = pr

    trivial :: Proof -> Proof
    trivial thm@(Complete _) = thm
    trivial pr@(Incomplete (Goal as f) cont)
        | TopFormula <- formulaId f = trivial $ next (Incomplete (Proven topI) cont)
        | f `elem` Map.elems as = trivial $ next (Incomplete (Proven $ aX f) cont)
        | otherwise = pr
    trivial _ = error "not a goal"
            

    split :: Proof -> Proof
    split thm@(Complete _) = thm
    split (Incomplete (Goal as f) cont) =
        case formulaId f of
            AndFormula f1 f2 -> pairToProof $ downleft (splitAnd f1 , CAndI1 cont $ splitAnd f2)
            IffFormula f1 f2 -> pairToProof $ downleft (Goal as $ impliesFormula f1 f2, CAndI1 cont $ Goal as $ impliesFormula f2 f1)
            NotFormula f1 -> let keys = Map.keys as in pairToProof $ downleft (Goal (Map.insert (head $ filter (`notElem` keys) allVars) f1 as) bottomFormula, CBotE f cont)
            _ -> error "mismatch formula"

      where
        splitAnd :: Formula-> ProofTree
        splitAnd f'
            | AndFormula f1 f2 <- formulaId f' = AndI (splitAnd f1) (splitAnd f2)
            | otherwise = Goal as f'
    split _ = error "not a goal"

    addFormula :: Bool -> Formula -> Proof -> Proof
    addFormula False f1 (Incomplete (Goal as f0) cont) = Incomplete (Goal as (andFormula f0 f1)) $ CAndE1 cont
    addFormula True f1 (Incomplete (Goal as f0) cont) = Incomplete (Goal as (andFormula f1 f0)) $ CAndE2 cont
    addFormula _ _ pr = pr

    chooseFormula :: Bool -> Proof -> Proof
    chooseFormula False (Incomplete (Goal as f0) cont) 
        | OrFormula f1 _ <- formulaId f0 = Incomplete (Goal as f1) $ COrI1 cont f1
        | otherwise = error "not an or"
    chooseFormula True (Incomplete (Goal as f0) cont) 
        | OrFormula _ f2 <- formulaId f0 = Incomplete (Goal as f2) $ COrI2 f2 cont
        | otherwise = error "not an or"
    chooseFormula _ pr = pr

    getExample :: Term -> Proof -> Proof
    getExample t (Incomplete (Goal as f0) cont) 
        | ExistsFormula <- formulaId f0 = Incomplete (Goal as $ freeVar t f0) $ CExistsI cont f0
        | otherwise = error "not an exists"
    getExample _ pr = pr

    getForall :: Proof -> Proof
    getForall (Incomplete (Goal as f0) cont)
        | ForallFormula <- formulaId f0 = Incomplete (Goal as $ freeVar (makeVar var) f0) $ CForallI var cont
        | otherwise = error "not an forall"
      where
        var = head $ freshVarsInFormulas $ f0 : Map.elems as
    getForall pr = pr

    getForalls :: Proof -> Proof
    getForalls pr@(Incomplete (Goal _ f0) _) 
        | ForallFormula <- formulaId f0 = getForalls $ getForall pr
        | otherwise = pr
    getForalls pr = pr



    qedM :: MonadFail m => Proof -> m Theorem
    qedM (Complete thm) = return thm
    qedM _ = fail "not complete"

    introM :: MonadFail m => Proof -> m Proof
    introM (Incomplete (Goal as f) cont) = do
                                        let name = head $ filter (`notElem` keys) allVars
                                        let f'  | ImpliesFormula f1 f2 <- formulaId f = return $ Incomplete (Goal (Map.insert name f1 as) f2) (CImpI f1 cont)
                                                | NotFormula f1 <- formulaId f = return $ Incomplete (Goal (Map.insert name f1 as) bottomFormula) (CImpI f1 cont)
                                                | otherwise = fail "not an implies"
                                        f'
            where keys = Map.keys as
    introM _ = fail "not a goal"

    introMs :: (MonadPlus m,MonadFail m) => Proof -> m Proof
    introMs pr = (introM pr >>= introMs) `mplus` return pr

   
    applyM :: MonadFail m => Formula -> Proof -> m Proof 
    applyM _ thm@(Complete _) = return thm
    applyM f (Incomplete (Goal as f0) cont) 
      | f0 == ff = return $ expandContext as f' cont flist
      -- | Just mp <- matchFormulas ff f0 = applyM (substManyInFormula mp f) pr
      | Just contAnd <- findAnd f0 ff cont = return $ Incomplete (Goal as ff) contAnd
      | Just contOr <- findOr f0 ff cont = return $ Incomplete (Goal as ff) contOr
      | otherwise =
        case formulaId ff of
            BottomFormula -> return $ expandContext as f' (CBotE f0 cont) flist
            NotFormula f1 -> return $ expandContext as f' (CBotE f0 cont) (f1 : flist)
            ExistsFormula -> return $ expandContext as f' (CExistsEE var cont $ Goal (Map.insert var (freeVar (makeVar var) ff) as) f0 ) flist
            OrFormula f1 f2 -> return $ expandContext as f' 
                (COrE0 cont (Goal (Map.insert var (freeVar (makeVar var) f1) as) f0) (Goal (Map.insert var (freeVar (makeVar var) f2) as) f0)) flist
            ForallFormula -> let terms = map (\case Just t -> t; Nothing -> makeSym "" []) $ getFormulaDiff ff f0 in 
                case findForall terms ff f0 cont of 
                    Just cont1 -> return $  expandContext as f' cont1 flist
                    Nothing -> fail "mismatched_formula_"
            IffFormula f1 f2 | f0 == f1 -> return $ expandContext as f' (CIffEI cont (Goal as f2)) flist
            IffFormula f1 f2 | f0 == f2 -> return $ expandContext as f' (CIffEI cont (Goal as f1)) flist
            _ -> fail "mismatched formula"
        where
            var = head $ filter (`notElem` keys) $ freshVarsInFormulas (f0 : ff : elems)
            elems = Map.elems as
            keys = Map.keys as
            (ff,flist,mp) = implist f0 [] f
            f' = maybe f (`substManyInFormula` f) mp
    applyM _ _ = fail "Not a goal"

    applyThmM :: MonadFail m => Theorem -> Proof -> m Proof
    applyThmM thm pr = do
        (Incomplete (Goal _ ff) cont) <- applyM (consequence thm) pr
        mp <- matchFormulas (consequence thm) ff
        return $ next $ Incomplete (Proven $ substManyInTheorem thm mp) cont

    applyAxiomM :: (MonadFail m,Axiom a) => a -> Proof -> m Proof
    applyAxiomM = applyThmM . fromAxiom

    applyAssM :: MonadFail m => String -> Proof -> m Proof
    applyAssM str pr@(Incomplete (Goal as _) _) = do
        Just f1 <- return $ Map.lookup str as
        (Incomplete (Goal _ ff) cont) <- applyM f1 pr
        True <- return (ff == f1)
        return $ next (Incomplete (Proven $ aX ff) cont)
    applyAssM _ pr = return pr

    trivialM :: MonadFail m => Proof -> m Proof
    trivialM thm@(Complete _) = return thm
    trivialM pr@(Incomplete (Goal as f) cont)
        | TopFormula <- formulaId f = trivialM $ next (Incomplete (Proven topI) cont)
        | f `elem` Map.elems as = trivialM $ next (Incomplete (Proven $ aX f) cont)
        | otherwise = return pr
    trivialM _ = fail "not a goal"
            

    splitM :: MonadFail m => Proof -> m Proof
    splitM thm@(Complete _) = return thm
    splitM (Incomplete (Goal as f) cont) =
        case formulaId f of
            AndFormula f1 f2 -> return $ pairToProof $ downleft (splitAnd f1 , CAndI1 cont $ splitAnd f2)
            IffFormula f1 f2 -> return $ pairToProof $ downleft (Goal as $ impliesFormula f1 f2, CAndI1 cont $ Goal as $ impliesFormula f2 f1)
            NotFormula f1 -> let keys = Map.keys as in return $ pairToProof $ downleft (Goal (Map.insert (head $ filter (`notElem` keys) allVars) f1 as) bottomFormula, CBotE f cont)
            _ -> fail "mismatch formula"

      where
        splitAnd :: Formula-> ProofTree
        splitAnd f'
            | AndFormula f1 f2 <- formulaId f' = AndI (splitAnd f1) (splitAnd f2)
            | otherwise = Goal as f'
    splitM _ = error "not a goal"

    addFormulaM :: MonadFail m => Bool -> Formula -> Proof -> m Proof
    addFormulaM False f1 (Incomplete (Goal as f0) cont) = return $ Incomplete (Goal as (andFormula f0 f1)) $ CAndE1 cont
    addFormulaM True f1 (Incomplete (Goal as f0) cont) = return $ Incomplete (Goal as (andFormula f1 f0)) $ CAndE2 cont
    addFormulaM _ _ _ = fail "not a goal"

    chooseFormulaM :: MonadFail m => Bool -> Proof -> m Proof
    chooseFormulaM False (Incomplete (Goal as f0) cont) 
        | OrFormula f1 f2 <- formulaId f0 = return $ Incomplete (Goal as f1) $ COrI1 cont f2
        | otherwise = fail "not an or"
    chooseFormulaM True (Incomplete (Goal as f0) cont) 
        | OrFormula f1 f2 <- formulaId f0 = return $ Incomplete (Goal as f2) $ COrI2 f1 cont
        | otherwise = fail "not an or"
    chooseFormulaM _ _ = fail "not a goal"

    getsM :: (MonadFail m,MonadPlus m,MonadInput m) => Proof -> m Proof
    getsM pr@(Incomplete (Goal _ f0) _)
        | ExistsFormula <- formulaId f0 = getExamplesM pr
        | ForallFormula <- formulaId f0 = getForallsM pr
        | ImpliesFormula _ _ <- formulaId f0 = introMs pr
        | otherwise = splitM pr
    getsM _ = fail "not a goal"
    getM :: (MonadFail m,MonadInput m) => Proof -> m Proof
    getM pr@(Incomplete (Goal as f0) cont)
        | ExistsFormula <- formulaId f0 = getExampleM pr
        | ForallFormula <- formulaId f0 = getForallM pr
        | AndFormula f1 f2 <- formulaId f0 = return $ pairToProof $ downleft (AndI (Goal as f1) (Goal as f2),cont)
        | ImpliesFormula _ _ <- formulaId f0 = introM pr
        | otherwise = splitM pr
    getM _ = fail "not a goal"

    getExampleM :: (MonadFail m,MonadInput m) => Proof -> m Proof
    getExampleM (Incomplete (Goal as f0) cont) = do
        ExistsFormula <- return $ formulaId f0
        t <- input TermF
        let f1 = freeVar t f0
        return $ Incomplete (Goal as f1) $ CExistsI cont f0
    getExampleM _ = fail "not a goal"

    getExamplesM :: (MonadPlus m,MonadFail m,MonadInput m) => Proof -> m Proof
    getExamplesM pr@(Incomplete (Goal as f0) cont) = do
        ExistsFormula <- return $ formulaId f0
        t <- input TermF
        let f1 = freeVar t f0
        getExamplesM $ Incomplete (Goal as f1) $ CExistsI cont f0
      `mplus` return pr
    getExamplesM _ = fail "not a goal"

    getForallM :: MonadFail m => Proof -> m Proof
    getForallM (Incomplete (Goal as f0) cont) = do
        ForallFormula <- return $ formulaId f0 
        return $ Incomplete (Goal as $ freeVar (makeVar var) f0) $ CForallI var cont
      where
        var = head $ freshVarsInFormulas $ f0 : Map.elems as
    getForallM pr = return pr

    getForallsM :: MonadFail m => Proof -> m Proof
    getForallsM pr@(Incomplete (Goal _ f0) _) 
        | ForallFormula <- formulaId f0 = getForallsM =<< getForallM pr
        | otherwise = return pr
    getForallsM _ = fail "not a goal"