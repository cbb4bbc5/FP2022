module Ex2 where

import qualified Data.Char as Char

data StreamTrans i o a
  = Return a
  | ReadS (Maybe i -> StreamTrans i o a)
  | WriteS o (StreamTrans i o a)

main :: IO ()
main = runIOStreamTrans toLower

toLower :: StreamTrans Char Char a
toLower = ReadS $ \x -> case x of
  Nothing -> toLower
  Just c  -> WriteS (Char.toLower c) toLower

runIOStreamTrans :: StreamTrans Char Char a -> IO a
runIOStreamTrans (Return a)   = return a
runIOStreamTrans (ReadS cont) = getChar >>= \c -> runIOStreamTrans $ cont (Just c)
runIOStreamTrans (WriteS c s) = putChar c >> runIOStreamTrans s

