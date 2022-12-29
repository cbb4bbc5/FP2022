import Data.Char
import System.IO

echoLower :: IO ()
echoLower = do b <- isEOF 
               if b 
                then return ()
                else do c <- getChar
                        putChar $ toLower c
                        echoLower
