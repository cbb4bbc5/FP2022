{-# LANGUAGE GeneralizedNewtypeDeriving,MultiParamTypeClasses,FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
module RefLangMonad where
    import RefLang
    import RefLangParser(parseExpr)
    import qualified Data.Map as Map
    import Control.Monad.ST
    import Data.STRef
    import Data.IORef
    import Control.Monad.State.Class
    import Control.Monad.Trans.Reader(ReaderT,runReaderT,ask)
    import Control.Monad.Trans.State(StateT(..),evalStateT)
    import Control.Monad.Trans.Class
    import Control.Monad.IO.Class
    


    -- zad 1
    type instance Loc (ST s) = STRef s (Value (ST s))

    instance MonadFresh (ST s) where
        freshLoc = newSTRef (VNum 0) 

    instance MonadHeap (ST s) where
        heapGet = readSTRef
        heapSet = writeSTRef
    type instance Loc IO = IORef (Value IO)

    instance MonadFresh IO where
        freshLoc = newIORef (VNum 0) 

    instance MonadHeap IO where
        heapGet = readIORef
        heapSet = writeIORef
    -- zad 2
    newtype FreshState m a = FreshState (StateT Integer m a)
      deriving (Functor,Applicative,Monad,MonadTrans,MonadIO,MonadFail)
    type instance Loc (FreshState m) = Integer
    instance Monad m => MonadFresh (FreshState m) where
        freshLoc = FreshState $ state (\s -> (s+1,s+1))

    evalFresh :: Monad m => FreshState m a -> m a
    evalFresh (FreshState s) = evalStateT s 0

    -- zad 4 : m (Value m) korzysta z wierzchniej monady 
    -- oraz z implementowanej monady
    -- można zmienić sygnatury na m (Value (Val m)) 
    --    gdzie Val to TypeFamily w MonadHeap
    --  i lift wyniki spowrotem do wierzchniej
    
    -- zad 3 
    newtype MapState s m a = 
        MapState (StateT (Map.Map s (Value (MapState s m))) m a)
            deriving (Functor, Applicative, Monad, MonadFail,MonadIO)
    instance MonadTrans (MapState s) where
        lift = MapState . lift
    type instance Loc (MapState s m) = s
    instance (MonadFail m, Ord s) => MonadHeap (MapState s m) where
        heapGet loc = MapState $ do
            dict <- get
            case Map.lookup loc dict of
                Nothing -> fail "Location Not Found"
                Just x -> return x
        heapSet loc val = MapState $ get >>= put . Map.insert loc val
    instance (MonadFresh m, Loc m ~ s) => MonadFresh (MapState s m) where
        freshLoc = lift freshLoc
    evalMap :: Monad m => MapState s m a -> m a
    evalMap (MapState st) = evalStateT st Map.empty

    -- zad 6
    newtype EnvReader m0 m a = EnvReader (ReaderT (Map.Map Var (Value (m0 (EnvReader m0 m)))) m a)
        deriving (Functor, Applicative, Monad, MonadFail, MonadIO)
    instance MonadTrans (EnvReader m0) where
        lift = EnvReader . lift
    instance MonadFail m => MonadEnv (EnvReader m0 m) where
        type EnvValue (EnvReader m0 m) = Value (m0 (EnvReader m0 m))
        lookupEnv v = EnvReader $ do
            dict <- ask
            case Map.lookup v dict of
                Nothing -> fail "Not found"
                Just x  -> return x
        extendEnv var val (EnvReader m) = EnvReader $ do
            dict <- ask
            lift $ runReaderT m (Map.insert var val dict)
    type instance Loc (EnvReader m0 m) = Loc m
    instance (MonadFresh m) => MonadFresh (EnvReader m0 m) where
        freshLoc = lift freshLoc
    instance (MonadEnv m,EnvValue m ~ Value (MapState s m)) => MonadEnv (MapState s m) where
        type EnvValue (MapState s m) = Value (MapState s m)
        lookupEnv = lift . lookupEnv
        extendEnv var val (MapState (StateT st)) = 
            MapState $ StateT $ \s -> (,s) <$> extendEnv var val (fst <$> st s)
    evalEnv :: EnvReader m0 m a -> Map.Map Var (Value (m0 (EnvReader m0 m))) -> m a
    evalEnv (EnvReader m) = runReaderT m


    parse str = let Right exp = parseExpr str in exp
    unFun (VFun f) = f
    example x = evalFresh $ 
       flip evalEnv 
        (Map.singleton "f" (VFun $ \(VNum n) -> return $ VNum $ 2*n)) $ 
         evalMap $ 
          eval (parse "\\x -> output 42;output 98;let y = f x in output y;x") 
           >>= ($ VNum x).unFun
