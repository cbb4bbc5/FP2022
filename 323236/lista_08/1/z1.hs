import Data.Char (toLower)
import System.IO (isEOF)

main :: IO ()
main = echoLower

echoLower = do
  eof <- isEOF
  if eof
    then return ()
    else getLine >>= putStrLn . map  toLower >> echoLower
