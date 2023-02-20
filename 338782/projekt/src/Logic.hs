
{-# LANGUAGE FlexibleContexts #-}
module Logic(Theorem(assumptions,consequence),
        forallI,forallE,forallEs,forallEt,
        existsI,existsE,
        aX,fromAxiom,
        andI,andLeftE,andRightE,
        orLeftI,orRightI,orE,
        impliesI,impliesE,
        iffI,iffE,
        topI,bottomE,
        substInTheorem,substManyInTheorem,
        forallIM,forallEM,forallEsM,
        existsIM,existsEM,
        andIM,andLeftEM,andRightEM,
        orLeftIM,orRightIM,orEM,
        impliesIM,impliesEM,
        iffIM,iffEM,axM,
        topIM,bottomEM,
        substInTheoremM,substManyInTheoremM) where
    import Formula
    import Axiom
    import qualified Data.Set as Set
    import Data.List(intercalate)
    import qualified Data.Map as Map
    import Control.Monad

    data Theorem = Theorem{assumptions :: Set.Set Formula, consequence :: Formula} deriving Eq
    instance Show Theorem where
        show (Theorem as f) = intercalate "\n" (map show $ Set.toList as) ++ " |- " ++ show f
    forallI :: String -> Theorem -> Theorem
    forallI var (Theorem as f) 
        | freeInFormulas var as = error "Variable free in assumptions"
        | otherwise = Theorem as $ forallFormula var f

    forallE :: Term -> Theorem -> Theorem
    forallE t (Theorem as f) 
        | ForallFormula <- formulaId f = Theorem as $ freeVar t f
        | otherwise = error "Not a forall"

    forallEt :: [Term] -> Theorem -> Theorem
    forallEt [] thm = thm
    forallEt (t:ts) thm@(Theorem _ f)
        | ForallFormula <- formulaId f = forallEt ts $ forallE t thm
        | otherwise = error "Not a forall"

    forallEs :: [String] -> Theorem -> Theorem
    forallEs [] thm = thm
    forallEs (t:ts) thm@(Theorem _ f)
        | ForallFormula <- formulaId f = forallEs ts $ forallE (read t) thm
        | otherwise = error "Not a forall"

    aX :: Formula -> Theorem
    aX f = Theorem (Set.singleton f) f

    fromAxiom :: Axiom ax => ax -> Theorem
    fromAxiom ax = Theorem Set.empty (axiom ax)

    andI :: Theorem -> Theorem -> Theorem
    andI (Theorem as1 f1) (Theorem as2 f2) = 
        Theorem (Set.union as1 as2) (andFormula f1 f2)

    andLeftE :: Theorem -> Theorem
    andLeftE (Theorem as f) 
        | AndFormula f1 _ <- formulaId f = Theorem as f1
        | otherwise = error "Not an and"
    andRightE :: Theorem -> Theorem
    andRightE (Theorem as f)
        | AndFormula _ f2 <- formulaId f = Theorem as f2
        | otherwise = error "Not an and"

    topI :: Theorem
    topI = Theorem Set.empty topFormula

    bottomI :: Theorem -> Theorem
    bottomI (Theorem as f) | NotFormula f1 <- formulaId f = Theorem as (impliesFormula f1 bottomFormula)
                           | otherwise = error "Not a not"

    bottomE :: Formula -> Theorem -> Theorem
    bottomE f (Theorem as f0) | BottomFormula <- formulaId f0, NotFormula f' <- formulaId f = Theorem (Set.delete f' as) f
                              | BottomFormula <- formulaId f0 = Theorem as f
                              | otherwise = error "Not a bottom"


    orRightI :: Theorem -> Formula -> Theorem
    orRightI (Theorem as f1) f2 = Theorem as $ orFormula f1 f2
    orLeftI :: Formula -> Theorem -> Theorem
    orLeftI f1 (Theorem as f2) = Theorem as $ orFormula f1 f2

    orE :: Theorem -> Theorem -> Theorem -> Theorem
    orE (Theorem as0 f0) (Theorem as1 f1) (Theorem as2 f2) 
        | OrFormula f01 f02 <- formulaId f0, f1==f2 
            = Theorem (as0 `Set.union` Set.delete f01 as1 `Set.union` Set.delete f02 as2) f1
        | otherwise = error "Not an or"

    impliesI :: Theorem -> Formula -> Theorem
    impliesI (Theorem as f1) f2 = Theorem (Set.delete f2 as) $ impliesFormula f2 f1
    
    impliesE :: Theorem -> Theorem -> Theorem
    impliesE thm1@(Theorem as1 fImp) thm2@(Theorem as2 f) 
        | ImpliesFormula f0 f1 <- formulaId fImp, f0 == f = Theorem (Set.union as1 as2) f1
        | NotFormula f1 <- formulaId fImp , f == f1 = impliesE (bottomI thm1) thm2
        | ImpliesFormula _ _ <- formulaId fImp = error "formulas dont match implies"
        | otherwise = error $ show thm1 ++ show thm2
                                                
    iffI :: Theorem -> Theorem -> Theorem
    iffI (Theorem as1 f01) (Theorem as2 f02) 
        | ImpliesFormula f11 f12 <- formulaId f01,ImpliesFormula f21 f22 <- formulaId f02,
            f11 == f22, f12 == f21 = Theorem (Set.union as1 as2) $ iffFormula f11 f12
        | otherwise = error "Formulas don't match"
    
    iffE :: Theorem -> Theorem -> Theorem
    iffE (Theorem as1 fImp) (Theorem as2 f) 
        | IffFormula f1 f2 <- formulaId fImp, f1 == f = Theorem (Set.union as1 as2) f2
        | IffFormula f1 f2 <- formulaId fImp, f2 == f = Theorem (Set.union as1 as2) f1
        | otherwise = error "Formulas don't match" 

    existsI :: Theorem -> Formula -> Theorem
    existsI (Theorem as f) f1 
            | ExistsFormula <- formulaId f1,
                (Just term:_) <- getFormulaDiff f1 f, f == freeVar term f1 = Theorem as f1
            | otherwise = error $ show f ++ " Formulas don't match " ++ show f1 ++ " : " ++ show (getFormulaDiff f1 f)
    
    existsE :: String -> Theorem -> Theorem -> Theorem
    existsE var (Theorem as0 f0) (Theorem as1 f1)
        | ExistsFormula <- formulaId f0, not (freeInFormula var f1 || freeInFormulas var ass || freeInFormula var f0) =
            Theorem (as0 `Set.union` ass) f1
        | freeInFormula var f1 = error "Variable free in consequence"
        | freeInFormulas var ass = error "Variable free in assumptions"
        | otherwise = error "Not an exists"
            where
                ass =  Set.delete (freeVar (makeVar var) f0) as1


    substInTheorem :: Theorem -> String -> Term -> Theorem
    substInTheorem (Theorem as f) var term = let subst = substInFormula var term in Theorem (Set.fromList $ map subst $ Set.toList as) $ subst f

    substManyInTheorem :: Theorem -> Map.Map String Term -> Theorem
    substManyInTheorem (Theorem as f) mp = let subst = substManyInFormula mp in Theorem (Set.fromList $ map subst $ Set.toList as) $ subst f

    axM :: (MonadFail m,MonadInput m) => m Theorem
    axM = aX <$> input FormulaF

    forallIM :: (MonadInput m,MonadPlus m) => Theorem -> m Theorem
    forallIM (Theorem as f) = do
        var <- input StringF
        guard $ not $ freeInFormulas var as
        return $ Theorem as $ forallFormula var f

    forallEM :: (MonadInput m,MonadFail m) => Theorem -> m Theorem
    forallEM (Theorem as f) = do
        t <- input TermF
        ForallFormula <- return $ formulaId f 
        return $ Theorem as $ freeVar t f
        

    forallEsM :: (MonadInput m,MonadFail m,MonadPlus m) => Theorem -> m Theorem
    forallEsM thm@(Theorem _ f) = 
        let more = do
                ForallFormula <- return $ formulaId f
                forallEM thm >>= forallEsM
        in more `mplus` return thm


    andIM :: MonadFail m => Theorem -> Theorem -> m Theorem
    andIM (Theorem as1 f1) (Theorem as2 f2) = 
        return $ Theorem (Set.union as1 as2) (andFormula f1 f2)

    andLeftEM :: MonadFail m => Theorem -> m Theorem
    andLeftEM (Theorem as f) = do
        AndFormula f1 _ <- return $ formulaId f 
        return $ Theorem as f1
    andRightEM :: MonadFail m => Theorem -> m Theorem
    andRightEM (Theorem as f) = do
        AndFormula _ f2 <- return $ formulaId f
        return $ Theorem as f2

    topIM :: MonadFail m => m Theorem
    topIM = return $ Theorem Set.empty topFormula
    
    bottomEM :: (MonadFail m,MonadInput m) => Theorem -> m Theorem
    bottomEM (Theorem as f0) = do
        f <- input FormulaF
        let res | BottomFormula <- formulaId f0, NotFormula f' <- formulaId f = return $ Theorem (Set.delete f' as) f
                | BottomFormula <- formulaId f0 = return $  Theorem as f
                | otherwise = fail "Not a bottom"
        res

    orRightIM :: MonadInput m => Theorem -> m Theorem
    orRightIM (Theorem as f1) = do
        f2 <- input FormulaF 
        return $ Theorem as $ orFormula f1 f2
    orLeftIM :: MonadInput m => Theorem -> m Theorem
    orLeftIM (Theorem as f2) = do
        f1 <- input FormulaF
        return $ Theorem as $ orFormula f1 f2

    orEM :: (MonadInput m,MonadFail m,MonadPlus m) => Theorem -> Theorem -> Theorem -> m Theorem
    orEM (Theorem as0 f0) (Theorem as1 f1) (Theorem as2 f2) = do
        OrFormula f01 f02 <- return $ formulaId f0
        guard $ f1==f2 
        return $ Theorem (as0 `Set.union` Set.delete f01 as1 `Set.union` Set.delete f02 as2) f1

    impliesIM :: MonadInput m => Theorem -> m Theorem
    impliesIM (Theorem as f1) = do
        f2 <- input FormulaF
        return $ Theorem (Set.delete f2 as) $ impliesFormula f2 f1
    
    impliesEM :: MonadFail m => Theorem -> Theorem -> m Theorem
    impliesEM thm1@(Theorem as1 fImp) thm2@(Theorem as2 f) 
        | ImpliesFormula f0 f1 <- formulaId fImp, f0 == f = return $ Theorem (Set.union as1 as2) f1        
        | NotFormula f1 <-  formulaId fImp, f==f1 = impliesEM (bottomI thm1) thm2
        | ImpliesFormula _ _ <- formulaId fImp = fail "formulas dont match implies"
        | otherwise = fail "Not an implies"
                                                
    iffIM :: MonadFail m => Theorem -> Theorem -> m Theorem
    iffIM (Theorem as1 f01) (Theorem as2 f02) 
        | ImpliesFormula f11 f12 <- formulaId f01,ImpliesFormula f21 f22 <- formulaId f02,
            f11 == f22, f12 == f21 = return $ Theorem (Set.union as1 as2) $ iffFormula f11 f12
        | otherwise = fail "Formulas don't match"
    
    iffEM :: MonadFail m => Theorem -> Theorem -> m Theorem
    iffEM (Theorem as1 fImp) (Theorem as2 f) 
        | IffFormula f1 f2 <- formulaId fImp, f1 == f = return $ Theorem (Set.union as1 as2) f2
        | IffFormula f1 f2 <- formulaId fImp, f2 == f = return $ Theorem (Set.union as1 as2) f1
        | otherwise = fail "Formulas don't match" 

    existsIM :: (MonadInput m,MonadFail m,MonadPlus m) => Theorem -> m Theorem
    existsIM (Theorem as f) = do
        f1 <- input FormulaF
        ExistsFormula <- return $ formulaId f1
        (Just term:_) <- return $ getFormulaDiff f1 f
        guard $ f == freeVar term f1
        return $ Theorem as f1
    
    existsEM :: (MonadInput m,MonadFail m,MonadPlus m) => Theorem -> Theorem -> m Theorem
    existsEM (Theorem as0 f0) (Theorem as1 f1) = do
            ExistsFormula <- return $ formulaId f0
            var <- input StringF
            let ass =  Set.delete (freeVar (makeVar var) f0) as1
            guard $ not (freeInFormula var f1 || freeInFormulas var ass || freeInFormula var f0)
            return $ Theorem (as0 `Set.union` ass) f1
            


    substInTheoremM :: MonadInput m => Theorem -> m Theorem
    substInTheoremM (Theorem as f) = do
        var <- input StringF
        term <- input TermF
        let subst = substInFormula var term in return $ Theorem (Set.fromList $ map subst $ Set.toList as) $ subst f

    substManyInTheoremM :: MonadFail m => Theorem -> Map.Map String Term -> m Theorem
    substManyInTheoremM (Theorem as f) mp = let subst = substManyInFormula mp in return $ Theorem (Set.fromList $ map subst $ Set.toList as) $ subst f





