{-# LANGUAGE FlexibleContexts, FlexibleInstances, FunctionalDependencies, ScopedTypeVariables #-}
module TwoPlayerGame where
    import Data.List
    import Control.Monad
    class Monad m => TwoPlayerGame m s a b | m -> s a b where
        moveA :: s -> m a
        moveB :: s -> m b

    data Score = AWins | Draw | BWins deriving (Eq,Show,Read)
    turn :: TwoPlayerGame m s a b => (s -> Maybe Score) -> (s -> a -> s) -> (s -> b -> s) -> s -> m Score
    turn wins makeMoveA makeMoveB s = do
        a <- moveA s
        let s1 = makeMoveA s a
        case wins s1 of
          Just win -> return win
          Nothing -> do
            b <- moveB s1
            let s2 = makeMoveB s1 b
            case wins s2 of
              Just win -> return win
              Nothing -> turn wins makeMoveA makeMoveB s2
    type Board4 = [[Score]]
    fourInARow :: TwoPlayerGame m Board4 Int Int => m Score
    fourInARow = turn wins (move AWins) (move BWins) (replicate 7 [])
        where
            wins :: Board4 -> Maybe Score
            wins s 
                | replicate 4 AWins `elem` fours = Just AWins
                | replicate 4 BWins `elem` fours = Just BWins
                | all ((7==).length) s = Just Draw
                | otherwise = Nothing
                    where
                        vert = concatMap (map (take 4).tails) s
                        hori = [ map (\x' -> s!!x'!!y) [x..x+3] | x <- [0..3], y <- [0..5], all (\x' -> length (s!!x') > y) [x..x+3]]
                        up  = [ map (\c -> s!!(x+c)!!(y+c)) [0..3] | x <- [0..3], y <- [0..2], all (\c -> length (s!!(x+c)) > (y+c)) [0..3]]
                        down  = [ map (\c -> s!!(x+c)!!(y-c)) [0..3] | x <- [0..3], y <- [3..5], all (\c -> length (s!!(x+c)) > y) [0..3]]
                        fours = concat [vert, hori, up, down]
            move :: Score -> [[Score]] -> Int -> [[Score]]
            move token s index = left++(x++[token]):right
                where
                    (left,x:right) = splitAt index s

    type Board9 = [[Score]]
    ticTacToe :: TwoPlayerGame m Board9 (Int,Int) (Int,Int) => m Score
    ticTacToe = turn wins (move AWins) (move BWins) (replicate 3 $ replicate 3 Draw)
        where
            wins :: Board9 -> Maybe Score
            wins s | replicate 3 AWins `elem` threes = Just AWins
                   | replicate 3 BWins `elem` threes = Just BWins
                   | Draw `notElem` concat s = Just Draw
                   | otherwise = Nothing
                where
                    threes = s++transpose s++[map (\i -> s!!i!!i) [0..2],map (\i -> s!!i!!(2-i)) [0..2]]
            move :: Score -> [[Score]] -> (Int,Int) -> [[Score]]
            move token s (x,y) = left++[up++token:down]++right
                where
                    (left,ys:right) = splitAt x s
                    (up,_:down) = splitAt y ys

    newtype IOGame s a b x = IOGame{runIOGame :: IO x}
    instance Functor (IOGame s a b) where
        fmap f = IOGame . fmap f . runIOGame
    instance Applicative (IOGame s a b) where
        pure = IOGame . pure
        mf <*> ma = mf >>= flip fmap ma
    instance Monad (IOGame s a b) where
        (IOGame m)>>=f = IOGame $ m>>=runIOGame.f
    instance (Show s, Read a, Read b) => TwoPlayerGame (IOGame s a b) s a b where
        moveA s = IOGame $ print s>>fmap read getLine
        moveB s = IOGame $ print s>>fmap read getLine

    data GameTree s a b x = 
        Return x  
        | ReadA s (a -> GameTree s a b x)
        | ReadB s (b -> GameTree s a b x)
    instance Functor (GameTree s a b) where
        fmap f (Return x) = Return (f x)
        fmap f (ReadA s g) = ReadA s (fmap f.g)
        fmap f (ReadB s g) = ReadB s (fmap f.g)
    instance Applicative (GameTree s a b) where
        pure = Return
        mf <*> ma = mf >>= flip fmap ma
    instance Monad (GameTree s a b) where
        Return x >>= f = f x
        ReadA s g >>= f = ReadA s (g >=> f)
        ReadB s g >>= f = ReadB s (g >=> f)
    instance TwoPlayerGame (GameTree s a b) s a b where
        moveA s = ReadA s Return
        moveB s = ReadB s Return

    play :: forall s a b. (Show s,Read a) => Int -> (s -> [a]) -> (s -> [b]) -> GameTree s a b Score -> IO ()
    play _ _ _ (Return x) = print x
    play depth aMoves bMoves (ReadA s f)
        = print s>>fmap read getLine >>= play depth aMoves bMoves . f
    play depth aMoves bMoves g@(ReadB _ f) 
      = play depth aMoves bMoves . f . snd . head $ simulate depth g
        where
            simulate :: Int -> GameTree s a b Score -> [(Score,b)]
            simulate _ (Return x) = [(x,undefined)]
            simulate 0 g = [(Draw,undefined)]
            simulate d (ReadA s f) = do
                let moves = do
                        a <- aMoves s
                        (outc,_) <- simulate (d-1) $ f a
                        return (outc,undefined)
                let win = [ w | w@(AWins,_) <- moves ]
                let draw = [ w | w@(Draw,_) <- moves ]
                let lose = [ w | w@(BWins,_) <- moves ]
                return $ head $ win++draw++lose
            simulate d (ReadB s f) = do
                let moves = do
                        b <- bMoves s
                        (outc,_) <- simulate (d-1) $ f b
                        return (outc,b)
                let win = [ w | w@(BWins,_) <- moves ]
                let draw = [ w | w@(Draw,_) <- moves ]
                let lose = [ w | w@(AWins,_) <- moves ]
                return $ head $ win++draw++lose

    moves4 :: Board4 -> [Int]
    moves4 s = filter ((/=7).length.(s!!)) [0..6]

    moves9 :: Board9 -> [(Int,Int)]
    moves9 s = [ (x,y) | x <- [0..2], y <- [0..2], s!!x!!y==Draw]



