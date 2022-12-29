import Data.Char(toLower)

echoLower :: IO ()
echoLower = do
  r <- getChar
  putChar (toLower r)
  echoLower

main = echoLower
