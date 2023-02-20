{-# LANGUAGE GADTs, GeneralisedNewtypeDeriving, StandaloneDeriving #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE TemplateHaskellQuotes #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses,FlexibleContexts #-}
module Monads(With(..),readS,parenLex,LexState,MapState,HisAccum,ProofReader,Handler,LinesMonad,TopProxy,
                MonadLex(..),MonadHandler(..),MonadMap(..),MonadRL(..),ProofRead(..),MonadPlusPlus(..),MonadTransPlus(..),Parse(..),
                evalLex,evalMap,evalHis,evalProof,evalHandler,evalLines,evalProxy,evalFull,
                deriveParse) where
    import Control.Applicative
    import Control.Monad
    import qualified Data.Map as Map
    import Control.Monad.Trans.Class
    import Control.Monad.State
    import qualified Control.Monad.Reader as Reader
    import Control.Monad.Reader(ReaderT(..),MonadReader)
    import qualified Control.Monad.Trans.Accum as Accum
    import Control.Monad.Accum
    import Proof
    import Formula
    import System.IO
    import Data.Maybe
    import Logic (Theorem(consequence))
    import Axiom
    import Language.Haskell.TH
    

    data With =
        GetProof (Proof,[Proof],[Proof])
        | GetFormat AnyFormat
    instance Show With where
        showsPrec 0 (GetFormat (LitA s)) s1 = show s ++ s1
        showsPrec 0 (GetProof (Complete thm,_,_)) s1 = show thm ++ s1
        showsPrec 0 (GetProof (pr,_,_)) s1 = show pr ++ s1
        showsPrec 0 (GetFormat f) s1 = show f ++ s1
        showsPrec _ w s1 = "(" ++ show w ++ ")" ++ s1

    newtype MapState m a = MState (StateT (Map.Map String With) m a)
        deriving (Functor, Applicative, Monad,Alternative, MonadPlus, MonadFail, MonadTrans, MonadIO, MonadRL , MonadLex, MonadHandler, MonadPlusPlus,ProofRead)
    deriving instance MonadAccum String m => MonadAccum String (MapState m) 
    deriving instance MonadReader (Maybe String) m => MonadReader (Maybe String) (MapState m) 
    newtype HisAccum m a = HAccum {unHAccum :: Accum.AccumT String m a}
        deriving (Functor, Applicative, Monad,Alternative, MonadPlus,MonadFail,MonadTrans, MonadIO, MonadLex,MonadInput,MonadHandler,MonadPlusPlus,MonadMap,ProofRead)
    deriving instance MonadReader (Maybe String) m => MonadReader (Maybe String) (HisAccum m) 
    newtype LexState m a = LexState {runLex :: StateT String m a}
        deriving (Functor,Applicative,Monad,MonadIO,MonadTrans,MonadRL,Alternative,MonadPlus,MonadFail,MonadInput,MonadHandler,MonadPlusPlus,MonadMap,ProofRead)
    deriving instance MonadAccum String m => MonadAccum String (LexState m) 
    deriving instance MonadReader (Maybe String) m => MonadReader (Maybe String) (LexState m) 
    newtype ProofReader m a = PReader (ReaderT (Maybe String) m a) 
        deriving (Functor, Applicative, Monad, MonadFail, Alternative, MonadPlus,MonadTrans,MonadIO,MonadHandler,MonadPlusPlus,MonadMap)
    deriving instance Monad m => MonadReader (Maybe String) (ProofReader m) 
    newtype Handler a = Handler (StateT (Maybe Handle) IO a)
        deriving (Functor,Applicative,Monad,MonadFail,Alternative,MonadPlus, MonadIO)
    newtype LinesMonad m a = LinesMonad ([String] -> [String] -> m (m (a,[String]),[String]))
    newtype TopProxy m a = TopProxy (m a)
        deriving (Functor, Applicative, Monad,Alternative, MonadPlus, MonadFail, MonadMap, MonadIO, MonadRL , MonadLex, MonadHandler, MonadPlusPlus,ProofRead)
    deriving instance MonadAccum String m => MonadAccum String (TopProxy m)
    deriving instance MonadReader (Maybe String) m => MonadReader (Maybe String) (TopProxy m)
    
    evalProxy :: TopProxy m a -> m a
    evalProxy (TopProxy st) = st
    evalLex :: Monad m => LexState m a -> m a
    evalLex (LexState st) = evalStateT st ""
    evalMap :: Monad m => MapState m a -> m a
    evalMap (MState st) = evalStateT st Map.empty
    evalHis :: Monad m => HisAccum m a -> m a
    evalHis (HAccum ac) = Accum.evalAccumT ac ""
    evalProof :: ProofReader m a -> m a
    evalProof (PReader r) = runReaderT r Nothing
    evalHandler :: Handler a -> IO a
    evalHandler (Handler st) = evalStateT st Nothing
    evalLines :: MonadPlus m => LinesMonad m a -> m a
    evalLines (LinesMonad l) = do
        (st,_) <- l [] []
        fst <$> st
    
    evalFull :: TopProxy (LexState (HisAccum (ProofReader (MapState (LinesMonad Handler))))) a -> IO a
    evalFull = evalHandler . evalLines . evalMap . evalProof . evalHis . evalLex . evalProxy

    class Monad m => MonadRL m where
        readLine :: m String
        writeStr :: String -> m ()
        writeLine :: String -> m ()
        writeLine = writeStr . (++"\n")
    class Monad m => MonadMap m where
        getMap :: String -> m (Maybe With)
        putMap :: String -> With -> m ()
        showMap :: m String
    class MonadRL m => MonadLex m where
        getLex :: m String
        reading :: Read a => ReadS a -> m a
    class MonadIO m => MonadHandler m where
        getHandle :: m (Maybe Handle)
        putHandle :: Maybe Handle -> m ()
    class Monad m => ProofRead m where
        ask :: m (Maybe String)
        local :: Maybe String -> m a -> m a
    class MonadPlus m => MonadPlusPlus m where
        (<||>) :: m a -> m a -> m a
        infixl 3 <||>
    class (forall m. (MonadRL m,MonadPlus m) => MonadPlus (t m)) => MonadTransPlus t where
        lift' :: (MonadRL m,MonadPlus m) => m a -> t m a
    class Parse a where
        parse :: (MonadInput m,MonadLex m,MonadFail m,MonadPlus m) => m a
    

    
    instance MonadRL m => MonadRL (Accum.AccumT String m) where
        readLine = lift readLine
        writeStr = lift . writeStr
    instance MonadLex m => MonadLex (Accum.AccumT String m) where
        getLex = lift getLex
        reading = lift . reading
    instance MonadRL m => MonadRL (StateT s m) where
        readLine = lift readLine
        writeStr = lift . writeStr
    instance MonadRL m => MonadRL (ReaderT s m) where
        readLine = lift readLine
        writeStr = lift . writeStr
    instance MonadMap m => MonadMap (StateT s m) where
        getMap = lift . getMap
        putMap  = (.) lift . putMap 
        showMap = lift showMap
    instance MonadMap m => MonadMap (ReaderT s m) where
        getMap = lift . getMap
        showMap = lift showMap
        putMap  = (.) lift . putMap 
    instance (MonadMap m,Monoid s) => MonadMap (Accum.AccumT s m) where
        getMap = lift . getMap
        showMap = lift showMap
        putMap  = (.) lift . putMap 
    instance MonadLex m => MonadLex (StateT s m) where
        getLex = lift getLex
        reading = lift . reading
    instance ProofRead m => ProofRead (StateT s m) where
        ask = lift ask
        local r m = StateT $ \s -> local r $ runStateT m s
    instance (Monoid s,ProofRead m) => ProofRead (Accum.AccumT s m) where
        ask = lift ask
        local r m = Accum.AccumT $ \s -> local r $ Accum.runAccumT m s
    instance MonadPlusPlus m => MonadPlusPlus (StateT s m) where
        StateT st1<||> StateT st2 = StateT $ \s -> st1 s <||> st2 s
    instance MonadPlusPlus m => MonadPlusPlus (ReaderT s m) where
        ReaderT st1<||> ReaderT st2 = ReaderT $ \s -> st1 s <||> st2 s
    instance (MonadPlusPlus m,Monoid s) => MonadPlusPlus (Accum.AccumT s m) where
        Accum.AccumT st1<||> Accum.AccumT st2 = Accum.AccumT $ \s -> st1 s <||> st2 s
    instance MonadLex m => MonadLex (ReaderT s m) where
        getLex = lift getLex
        reading = lift . reading
    instance Monad m => MonadAccum String (HisAccum m) where
        add s = HAccum $ Accum.add $ '\n' : s 
        look = HAccum Accum.look
    instance Monad m => MonadMap (MapState m) where
        getMap s = MState $ gets (Map.lookup s) 
        putMap s w = MState $ modify $ Map.insert s w
        showMap = MState $ gets $ showInner . Map.assocs
            where
                showInner [] = []
                showInner ((label,cont):xs) =
                    let strCont = show cont in 
                    replicate (length (takeWhile (/='\n') strCont) + length label)
                        '-' ++ "\n" ++label ++":\n" ++ strCont ++ "\n" ++ showInner xs
    instance MonadTransPlus (StateT s) where
        lift' = lift
    instance Monoid s => MonadTransPlus (Accum.AccumT s) where
        lift' = lift
    instance MonadTransPlus (ReaderT s) where
        lift' = lift
    instance MonadRL m => MonadRL (HisAccum m) where
        readLine = HAccum $ do
            l <- readLine
            unHAccum $ add l
            return l
        writeStr = lift . writeStr

    readS :: Read a => ReadS a
    readS = readsPrec 11

    parenLex :: (MonadLex m,MonadFail m,MonadPlus m) => m a -> m a
    parenLex m = do{"(" <- getLex;m' <- m;")" <- getLex;return m'} <|> m

    instance (MonadPlus m,MonadFail m,MonadLex m,MonadMap m,ProofRead m) => MonadInput (TopProxy m) where
        input format = 
          let fromMap = TopProxy $ do
                Just x <- getLex >>= getMap
                case (x, format) of
                    (GetFormat (LitA s),StringF) -> return s
                    (GetFormat f,FormulaF) -> evalProxy $ evalFormat f readFormula 
                    (GetFormat f,TermF) -> evalProxy $ evalFormat f readTerm
                    (GetProof (pr,_,_),FormulaF) -> maybe mzero return $ consequence <$> qed pr <|> snd <$> goal pr
                    _ -> mzero
              frmt = case format of
                FormulaF -> reading readS
                StringF -> reading readS
                TermF -> reading readS
                _ -> mzero
              fromAss = case format of
                    FormulaF -> do
                        Just name <- ask
                        Just (GetProof (pr,_,_)) <- getMap name
                        as <- getLex
                        Just (ass,_) <- return $ goal pr
                        Just f <- return $ lookup as ass
                        return f
                    _ -> mzero
                
            in parenLex (fromAss <|> fromMap <|> frmt)
        inputFormat =  
          let fromMap = do
                x <- getLex >>= getMap
                case x of
                    Just (GetFormat f) -> return f
                    _ -> mzero
            in parenLex (fromMap <|> reading readS)

    instance MonadRL Handler where
        readLine = (getHandle >>= liftIO . hGetLine . fromMaybe stdin) <|> ( getHandle >>= liftIO . hClose . fromJust >> putHandle Nothing >> readLine)
        writeStr = liftIO . putStr

    instance (MonadHandler m,MonadRL m,MonadMap m,MonadFail m,MonadPlus m) => MonadRL (ProofReader m) where
        writeStr = lift . writeStr
        readLine = PReader $ do
            do Just s0 <- Reader.ask 
               Just pr <- getMap s0
               writeLine "----------------"
               writeLine $ s0 ++ ":"
               writeLine $ show pr
             <|> writeLine ">"
            l <- readLine
            do Just _ <- getHandle
               writeLine l
             <|> return ()
            return l

    instance (MonadRL m,MonadFail m) => MonadLex (LexState m) where
        getLex = LexState $ do
            s <- get
            case lex s of
                [] -> writeLine "fail">> fail ""
                [("","")] -> readLine >>= put >> runLex getLex
                [("\\",_)] -> readLine >>= put >> runLex getLex
                ((s1,s2):_) -> put s2 >> return s1
        reading rea = LexState $ checkNewLine >> do
            str <- get
            case rea str of
                [] -> fail ""
                xss | (x,xs) <- last xss -> put xs >> return x
          where
            checkNewLine :: StateT String m ()
            checkNewLine = do
                str <- get
                case lex str of
                    [("\\",_)] -> readLine >>= put >> checkNewLine
                    _ -> return ()

    instance MonadHandler Handler where
        getHandle = Handler get
        putHandle = Handler . put
    instance Monad m => ProofRead (ProofReader m) where
        ask = PReader Reader.ask
        local r (PReader m) = PReader $ Reader.local (const r) m
    instance MonadHandler m => MonadHandler (StateT s m) where
        getHandle = lift getHandle
        putHandle = lift . putHandle
    instance MonadHandler m => MonadHandler (ReaderT s m) where
        getHandle = lift getHandle
        putHandle = lift . putHandle
    instance (MonadHandler m,Monoid s) => MonadHandler (Accum.AccumT s m) where
        getHandle = lift getHandle
        putHandle = lift . putHandle

    instance MonadPlus m => Functor (LinesMonad m) where
        fmap f (LinesMonad m) = LinesMonad $ \xs0 s0 -> do
            (st,xs) <- m xs0 s0
            do (a,s1) <- st
               pure (pure (f a,s1),xs)
             <|> pure (mzero,xs)
        
    instance (MonadRL m,MonadPlus m) => Applicative (LinesMonad m) where
        pure a = LinesMonad $ \xs s0 -> pure (pure (a,s0),xs)
        (<*>) = ap
    instance (MonadRL m,MonadPlus m) => Monad (LinesMonad m) where
        (LinesMonad m) >>= f = LinesMonad m1
            where
                m1 xs0 s0 = do
                    (st,xs) <- m xs0 s0
                    do
                        (a,s1) <- st
                        let LinesMonad ys = f a
                        ys xs s1
                      <|> return (mzero,xs)
    instance (MonadRL m,MonadPlus m) => Alternative (LinesMonad m) where
        empty = LinesMonad $ \xs _ -> pure (mzero,xs)
        LinesMonad m1 <|> LinesMonad m2 = LinesMonad m'
          where
            m' xs0 s0 = do
                (st1,xs1) <- m1 [] s0
                (as',xs') <- (st1 >>= \as' -> return (return as',xs1)) <|> do
                    (st2,xs2) <- m2 [] $ s0 ++ xs1
                    (st2 >>= \as' -> return (return as',xs2)) <|> return (mzero,xs1++xs2) 
                return (as',xs0 ++ xs')
    instance (MonadRL m,MonadPlus m) => MonadPlus (LinesMonad m) where
    instance (MonadRL m,MonadPlus m) => MonadFail (LinesMonad m) where
        fail _ = mzero
    instance MonadTransPlus LinesMonad where
        lift' m = LinesMonad $ \xs0 s0 -> return ((,s0) <$> m,xs0)
    instance (MonadRL m,MonadPlus m,MonadIO m) => MonadIO (LinesMonad m) where
        liftIO = lift' . liftIO
    instance (MonadRL m,MonadPlus m) => MonadRL (LinesMonad m) where
        readLine = LinesMonad $ \xs0 s0 -> do
            case s0 of
                [] -> do
                    l <- readLine
                    return (return (l,s0),xs0 ++ [l])
                l : xs -> return (return (l,xs),xs0)
        writeStr = lift' . writeStr
    instance (MonadRL m,MonadPlus m) => MonadPlusPlus (LinesMonad m) where
        LinesMonad m1 <||> LinesMonad m2 = LinesMonad m'
            where
                m' xs0 s0 = do
                    (as1,xs) <- do{
                        ;(st,xs) <- m1 [] s0
                        ;(a,s1) <- st
                        ;return (return (a,s1),xs)} <|> do{
                            ;(st,xs) <- m2 [] s0
                            ;(a,s1) <- st
                            ;return (return (a,s1),xs)} <|> return (mzero,[])
                    return (as1,xs0 ++ xs)

    instance (MonadRL m,MonadPlus m,MonadHandler m) => MonadHandler (LinesMonad m) where
        getHandle = lift' getHandle
        putHandle = lift' . putHandle



    deriveParse :: Name -> Q [Dec]
    deriveParse t = do
        TyConI (DataD _ _ _ _ constrs _) <- reify t
        let readCons (NormalC name fields) = do
                let consName = nameBase name
                let xx [] = return ([],[])
                    xx (x:xs) = do
                        x1 <- newName "x"
                        ConT name' <- return x
                        (x1s,xs') <- xx xs
                        let bname = nameBase name'
                        return (VarE x1 : x1s , BindS (VarP x1) (AppE (VarE $ mkName "input") (ConE $ mkName $ bname++"F")) : xs')
                (x1s,xs) <- xx $ map snd fields
                let doexpr es = DoE Nothing (BindS (LitP (StringL consName)) (VarE $ mkName "getLex") : xs ++ es)
                let apps [] acc = doexpr [NoBindS $ AppE (VarE $ mkName "return") $ acc $ ConE name]
                    apps (a : as) acc = apps as $ \cc -> acc (AppE cc a)
                return $ apps (reverse x1s) id
            readCons _ = fail "unusual constructor"
        xe <- mapM readCons constrs
        let appplus [] x0 = x0
            appplus (x:xs) x0 = appplus xs $ AppE (AppE (VarE $ mkName "mplus") x0) x
        return [InstanceD Nothing [] (AppT (ConT ''Parse) (ConT t)) [ValD (VarP $ mkName "parse") (NormalB $ appplus xe $ VarE $ mkName "mzero") []]]

    instance (Parse a,Parse b) => Parse (TwoAxioms a b) where
        parse = (FirstAxiom <$> parse) `mplus` (SecondAxiom <$> parse) 
