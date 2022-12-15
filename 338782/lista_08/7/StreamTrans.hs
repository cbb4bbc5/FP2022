module StreamTrans where    

    import Data.Functor
    import Data.Function
    import Control.Monad

    data StreamTrans i o a 
        = Return a
        | ReadS (Maybe i -> StreamTrans i o a)
        | WriteS o (StreamTrans i o a)
    
-- zadanie 2

    lower :: Char -> Char
    lower c = if 'A' <= c && c <= 'Z' 
                then (toEnum.(+ (fromEnum 'a' - fromEnum 'A')).fromEnum)c
                else c

    toLower :: StreamTrans Char Char ()
    toLower = ReadS low where
        low Nothing = Return ()
        low (Just c) = WriteS (lower c) toLower

    runIOStreamTrans :: StreamTrans Char Char a -> IO a
    runIOStreamTrans (Return x) = return x
    runIOStreamTrans (ReadS f) = getChar>>=runIOStreamTrans.f.Just
    runIOStreamTrans (WriteS c nxt) = putChar c>>runIOStreamTrans nxt

--zadanie 3

    listTrans :: StreamTrans i o a -> [i] -> ([o],a)
    listTrans (Return x) _ = ([],x)
    listTrans (ReadS f) [] = listTrans (f Nothing) []
    listTrans (ReadS f) (x:xs) = listTrans (f (Just x)) xs
    listTrans (WriteS o st) xs = 
        let (os,a) = listTrans st xs in
            (o:os,a)

--zadanie 4

    runCycle :: StreamTrans a a b -> b
    runCycle st = let (os,x) = listTrans st os in x

--zadanie 5


    (|>|) :: StreamTrans i m a -> StreamTrans m o b -> StreamTrans i o b
    st1 |>| (Return b) = Return b
    st1 |>|  (WriteS o st2) = WriteS o (st1 |>| st2)
    (ReadS f) |>| st2 = ReadS  (\i -> f i |>| st2)
    st1@(Return _) |>| (ReadS f) = st1 |>| f Nothing
    (WriteS o st1) |>| (ReadS f) = st1 |>| f (Just o)

    -- zadanie 6


    catchOutput :: StreamTrans i o a -> StreamTrans i b ([o],a)
    catchOutput (WriteS o st) = catchOutput' (catchOutput st) o
        where
            catchOutput' (Return (os,x)) o = Return (o:os,x)
            catchOutput' (ReadS f) o =     ReadS (\i -> catchOutput' (f i) o)
            catchOutput' s o = s
    {-catchOutput (WriteS o st) = do
        (os,a) <- catchOutput st
        return (o:os,a)
    -}
    catchOutput (Return x) = Return ([],x)
    catchOutput (ReadS f) = ReadS (catchOutput.f)

    -- zadanie 7

    data BF
        = MoveR -- >
        | MoveL -- <
        | Inc -- +
        | Dec -- -
        | Output -- .
        | Input -- ,
        | While [BF] -- [ ]

    brainfuckParser :: StreamTrans Char BF ()
    brainfuckParser = ReadS mCharToBF
        where 
            mCharToBF Nothing = Return ()
            mCharToBF (Just '>') = WriteS MoveR brainfuckParser
            mCharToBF (Just '<') = WriteS MoveL brainfuckParser
            mCharToBF (Just '+') = WriteS Inc brainfuckParser
            mCharToBF (Just '-') = WriteS Dec brainfuckParser
            mCharToBF (Just '.') = WriteS Output brainfuckParser
            mCharToBF (Just ',') = WriteS Input brainfuckParser
            mCharToBF (Just ']') = Return ()
            mCharToBF (Just '[') = 
                catchOutput brainfuckParser >>= 
                    flip WriteS brainfuckParser . While . fst
            mCharToBF (Just _) = brainfuckParser


    -- zadanie 8


    runBF :: [BF] -> StreamTrans Char Char ()
    runBF xs = evalBFBlock (repeat 0,repeat 0) xs $> ()

    type Tape = ([Integer],[Integer])
    evalBF :: Tape -> BF -> StreamTrans Char Char Tape
    evalBF (x:xs,ys) MoveL = Return (xs,x:ys)
    evalBF (xs,y:ys) MoveR = Return (y:xs,ys)
    evalBF (xs,y:ys) Inc = Return (xs,mod (y+1) 256:ys)
    evalBF (xs,y:ys) Dec = Return (xs,mod (y-1) 256:ys)
    evalBF p@(_,y:_) Output = WriteS (coerseEnum y) (Return p)
    evalBF p@(xs,_:ys) Input = ReadS readInput
        where 
            readInput Nothing = Return p
            readInput (Just i) = Return (xs,coerseEnum i:ys)
    evalBF tape@(_,y:_) exp@(While xs) =
        if y==0 then Return tape
                    else evalBFBlock tape xs >>= flip evalBF exp
    evalBF tape _ = Return tape

    evalBFBlock :: Tape -> [BF] -> StreamTrans Char Char Tape
    evalBFBlock = foldM evalBF

    coerseEnum :: (Enum a,Enum b) => a -> b
    coerseEnum = toEnum . fromEnum

    -- zadanie 9 

    instance Functor (StreamTrans i o) where
        fmap f m = m >>= return . f
    instance Applicative (StreamTrans i o) where
        pure = return
        m1 <*> m2 =  do{ x1 <- m1; x2 <- m2; return (x1 x2)}
    instance Monad (StreamTrans i o) where
        return = Return
        (Return a) >>= f = f a
        (ReadS f0) >>= f = ReadS (f0 >=> f)
        (WriteS o st) >>= f = WriteS o (st >>= f)
