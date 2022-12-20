module Main (main, Main.toLower, runIOStreamTrans) where

import Data.Char(toLower)
data StreamTrans i o a = 
  Return a
  | ReadS (Maybe i -> StreamTrans i o a)
  | WriteS o (StreamTrans i o a)

toLower :: StreamTrans Char Char () 
toLower = ReadS (\r -> case r of 
												Nothing -> Return () 
												Just r -> WriteS (Data.Char.toLower r) Main.toLower)

-- ReadS f -> czytamy wartość z istream i przekazujemy do f
-- WriteS x c -> piszemy x na ostream i uruchamiamy c
-- Return a -> kończymy obliczenia zwróceniem a
runIOStreamTrans :: StreamTrans Char Char a -> IO a 
runIOStreamTrans (Return a) = return a 
runIOStreamTrans (ReadS f) = getChar >>= (\r -> runIOStreamTrans (f (Just r))) 
runIOStreamTrans (WriteS x c) = putChar x >>=  (\_ -> runIOStreamTrans c)

main = runIOStreamTrans Main.toLower
