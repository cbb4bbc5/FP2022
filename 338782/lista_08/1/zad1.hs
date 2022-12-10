import GHC.IO.Handle (hGetChar)
import System.Process.Internals (ProcRetHandles(hStdOutput))
echoLower :: IO ()
echoLower = do
    c <- getChar
    (putChar.if 'A' <= c && c<='Z' 
        then toEnum.(+(fromEnum 'a' - fromEnum 'A')).fromEnum
        else id) c
    echoLower
