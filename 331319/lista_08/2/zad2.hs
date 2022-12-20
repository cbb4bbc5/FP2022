import qualified Data.Char as C
import System.IO
import Control.Monad

data StreamTrans i o a = 
      Return a 
    | ReadS (Maybe i -> StreamTrans i o a)
    | WriteS o (StreamTrans i o a)

-- Zad. 2.
toLower :: StreamTrans Char Char () -- zły typ na liście?
toLower = ReadS (\ m -> case m of 
                         Nothing -> Return ()
                         Just i  -> WriteS (C.toLower i) toLower)

runIOStreamTrans :: StreamTrans Char Char a -> IO a 
runIOStreamTrans (Return a) = return a 
runIOStreamTrans (ReadS f)  = do c <- getChar
                                 if c == '\04'
                                    then runIOStreamTrans $ f Nothing 
                                    else do runIOStreamTrans $ f (Just c) 
runIOStreamTrans (WriteS x s) = do putChar x
                                   runIOStreamTrans s 

-- Zad. 3.
listTrans :: StreamTrans i o a -> [i] -> ([o], a)
listTrans (Return a) xs = ([], a)
listTrans (ReadS f)  [] = listTrans (f Nothing) []
listTrans (ReadS f) (x:xs) = listTrans (f (Just x)) xs 
listTrans (WriteS x s) xs = 
    let (res, a) = listTrans s xs in (x:res, a)

-- Zad. 4. 
runCycle :: StreamTrans a a b -> b 
runCycle (Return a) = a 
runCycle (ReadS f) = runCycle (ReadS f)
runCycle (WriteS x (ReadS f)) = runCycle $ f (Just x)
runCycle (WriteS x s) = runCycle s 

-- Zad. 5. 
(|>|) :: StreamTrans i m a -> StreamTrans m o b -> StreamTrans i o b
s |>| Return b  = Return b
s |>| WriteS x s' = WriteS x $ s |>| s'
-- W przypadku (Read, Read) lub (Return, Read) - nie ma co czytać
WriteS x s |>| ReadS f = s |>| (f $ Just x)

-- Zad. 6.
catchOutput :: StreamTrans i o a -> StreamTrans i b (a, [o])
catchOutput = foldST (:) [] 

foldST :: (o -> [o] -> [o]) -> [o] -> StreamTrans i o a -> StreamTrans i b (a, [o])
foldST f acc (Return a) = Return (a, reverse acc)
foldST f acc (WriteS x s) = foldST f (f x acc) s
foldST f acc (ReadS g) = ReadS $ foldST f acc . g

-- Zad. 9.
{-
joinST :: StreamTrans i o (StreamTrans i o a) -> StreamTrans i o a 
joinST (Return sa) = sa 
joinST (ReadS f) = ReadS (\ m -> joinST $ f m)
joinST (WriteS x s) = WriteS x (joinST s)
-}
instance Functor (StreamTrans i o) where 
    fmap f (Return a)   = Return $ f a 
    fmap f (ReadS g)    = ReadS $ \ m -> fmap f (g m)
    fmap f (WriteS x s) = WriteS x $ fmap f s 

instance Applicative (StreamTrans i o) where 
    pure  = Return 
    (<*>) = ap 
    
instance Monad (StreamTrans i o) where
    return = pure
    Return a   >>= f   = f a 
    ReadS f    >>= g   = ReadS $ \ m -> f m >>= g 
    WriteS x s >>= f   = WriteS x $ s >>= f 

-- Test
inc :: Char -> Char 
inc = C.chr . (+) 1 . C.ord 

shiftByOne :: StreamTrans Char Char Int
shiftByOne = ReadS (\m -> case m of 
                            Nothing -> Return 2137
                            Just i  -> WriteS (inc i) shiftByOne)

toLowerAndShift = do u <- toLower 
                     v <- shiftByOne
                     return v

-- Zad. 7.
data BF = 
      MoveR      -- >
    | MoveL      -- <
    | Inc        -- +
    | Dec        -- -
    | Output     -- .
    | Input      -- ,
    | While [BF] -- [ ]
    deriving Show

bfDict :: [(Char, BF)]
bfDict = [('>', MoveR), ('<', MoveL), ('+', Inc), ('-', Dec), ('.', Output), (',', Input)]

brainfuckParser :: StreamTrans Char BF ()
brainfuckParser = ReadS $ \ m -> case m of 
                                    Nothing -> Return ()
                                    Just c  -> if c == '['
                                                then do 
                                                    r <- catchOutput brainfuckParser
                                                    WriteS (While $ snd r) brainfuckParser
                                                else if c == ']'
                                                    then Return ()
                                                    else case lookup c bfDict of 
                                                            Nothing -> brainfuckParser
                                                            Just bf -> WriteS bf brainfuckParser
-- Nie sprawdzam niedomkniętych nawiasów.

-- Zad. 8.
coerceEnum :: (Enum a, Enum b) => a -> b
coerceEnum = toEnum . fromEnum

type Tape = ([Integer], [Integer]) -- element pod kursorem to głowa prawej listy
evalBF :: Tape -> BF -> StreamTrans Char Char Tape 
evalBF ([], []) _ = Return ([], [])
evalBF (l, hd:r) MoveR = Return (hd:l, r)
evalBF (hd:l, r) MoveL = Return (l, hd:r)
evalBF (l, hd:r) Inc   = Return (l, (hd + 1):r)
evalBF (l, hd:r) Dec   = Return (l, (hd - 1):r)
evalBF (l, _:r)  Input = ReadS $ \ m -> case m of 
                                            Nothing -> Return ([], []) -- koniec programu
                                            Just c  -> Return (l, (coerceEnum c):r) 
evalBF tp@(_, hd:_) Output = WriteS (coerceEnum hd) $ Return tp
evalBF tp@(_, hd:_) (While bfs) = 
    if hd /= 0
        then evalBFBlock tp bfs >>= \ tp' -> evalBF tp' (While bfs)
        else Return tp

evalBFBlock :: Tape -> [BF] -> StreamTrans Char Char Tape 
evalBFBlock = foldM evalBF

runBF :: [BF] -> StreamTrans Char Char () 
runBF bfs = evalBFBlock ([0,0..], [0,0..]) bfs >>= \ _ -> Return ()

testBF :: IO ()
testBF = runIOStreamTrans (catchOutput brainfuckParser >>= \ (_, bfs) -> runBF bfs) >>= \ _ -> putChar '\n'
