module Ex1 where

import Data.Char (toLower)
import System.IO (isEOF)

main :: IO ()
main = echoLower

echoLower :: IO ()
echoLower = do
  eof <- isEOF
  if eof
    then return ()
    else do
      line -> getLine
      putStrLn $ map toLower line
      echoLower

