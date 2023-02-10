{-# LANGUAGE TupleSections, FlexibleContexts, FlexibleInstances, FunctionalDependencies #-}
module BFTape where
    import Control.Monad
    import Control.Applicative
    import Data.Bifunctor
    data BF
        = MoveR -- >
        | MoveL -- <
        | Inc -- +
        | Dec -- -
        | Output -- .
        | Input -- ,
        | While [BF] -- [ ]
    class Monad m => TapeMonad m a | m -> a where
        tapeGet :: m a
        tapePut :: a -> m ()
        moveLeft :: m ()
        moveRight :: m ()

    evalBFTape :: TapeMonad m Integer => [BF] -> [Char] -> m [Char]
    evalBFTape [] xs = return []
    evalBFTape (MoveR:bfs) xs = moveRight>>evalBFTape bfs xs
    evalBFTape (MoveL:bfs) xs = moveLeft>>evalBFTape bfs xs
    evalBFTape (Inc:bfs) xs = tapeGet>>=tapePut.(+1)>>evalBFTape bfs xs
    evalBFTape (Dec:bfs) xs = tapeGet>>=tapePut.subtract 1>>evalBFTape bfs xs
    evalBFTape (Output:bfs) xs = do
        o <- tapeGet
        os <- evalBFTape bfs xs
        return ((toEnum.fromEnum) (mod o 256):os)
    evalBFTape (Input:bfs) [] = evalBFTape bfs []
    evalBFTape (Input:bfs) (x:xs) = tapePut ((toInteger.fromEnum) x)>>evalBFTape bfs xs
    evalBFTape bf@(While bfs1:bfs) xs = do
        n <- tapeGet
        if n==0 
            then evalBFTape bfs xs
            else evalBFTape (bfs1++bf) xs 
    
    newtype Identity a = Identity {unIdentity::a}
    instance Functor Identity where
        fmap f (Identity x) = Identity $ f x
    instance Applicative Identity where
        pure = Identity
        Identity f <*> Identity a = Identity $ f a
    instance Monad Identity where
        (Identity x) >>= f = f x
    newtype StateT s m a = StateT {runStateT :: s -> m (a,s)}
    type State s a = StateT s Identity a
    instance Functor m => Functor (StateT s m) where
        fmap f = StateT . (.) (fmap $ first f) . runStateT
    instance Monad m => Applicative (StateT s m) where
        pure x = StateT $ pure . (x,)
        (<*>) = ap
    instance Monad m => Monad (StateT s m) where
        return = pure
        StateT m >>= f = StateT $ \s -> do
            (a,s1) <- m s
            runStateT (f a) s1
    instance MonadPlus m => Alternative (StateT s m) where
        empty = StateT $ const empty
        StateT a <|> StateT b = StateT $ \s -> a s <|> b s
    instance MonadPlus m => MonadPlus (StateT s m) where
    instance MonadFail m => MonadFail (StateT s m) where
        fail s = StateT $ \_ -> fail s
    put :: Monad m => s -> StateT s m ()
    put = StateT . const . return  . ((),)
    get :: Monad m => StateT s m s
    get = StateT $ \s -> return (s,s)
    runState :: State s a -> s -> (a,s)
    runState = (.) unIdentity.runStateT
    evalStateT :: Functor m => StateT s m a -> s -> m a
    evalStateT = (.) (fmap fst) . runStateT 
    evalState :: State s a -> s -> a
    evalState = (.) unIdentity . evalStateT
    instance TapeMonad (StateT ([i],[i]) Identity) i where
        tapeGet = do
            s <- get
            let (ls,x:rs) = s
            return x
        tapePut x = do
            s <- get
            let (ls,_:rs) = s
            put (ls,x:rs)
        moveLeft = do
            s <- get
            let (ls,x:rs) = s
            put (x:ls,rs)
        moveRight = do
            s <- get
            let (x:ls,rs) = s
            put (ls,x:rs)
    runBFTape :: [BF] -> [Char] -> [Char]
    runBFTape bf xs = evalState (evalBFTape bf xs :: State ([Integer],[Integer]) [Char]) (repeat 0,repeat  0)

    lift m = StateT $ \s -> fmap (,s) m
    instance TapeMonad m a => TapeMonad (StateT [Char] m) a where
        tapeGet = lift tapeGet
        tapePut a = lift $ tapePut a
        moveLeft  = lift moveLeft
        moveRight = lift moveRight

    evalBF :: TapeMonad m Integer => [BF] -> StateT [Char] m [Char]
    evalBF [] = return []
    evalBF (MoveR:bfs) = moveRight>>evalBF bfs
    evalBF (MoveL:bfs) = moveLeft>>evalBF bfs
    evalBF (Inc:bfs) = tapeGet>>=tapePut.(+1)>>evalBF bfs
    evalBF (Dec:bfs) = tapeGet>>=tapePut.subtract 1>>evalBF bfs
    evalBF (Output:bfs) = do
        o <- tapeGet
        os <- evalBF bfs
        return ((toEnum.fromEnum) (mod o 256):os)
    evalBF (Input:bfs) = get>>=\ (x:xs->tapePut ((toInteger.fromEnum) x)>>evalBF bfs xs
    evalBF bf@(While bfs1:bfs) xs = do
        n <- tapeGet
        if n==0 
            then evalBFTape bfs xs
            else evalBFTape (bfs1++bf) xs 
    
    

