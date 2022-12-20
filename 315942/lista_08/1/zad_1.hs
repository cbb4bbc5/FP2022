import Data.Char

chrEchoLower = do
    echo <- getChar
    let lower = toLower echo
    putChar lower
    chrEchoLower

strEchoLower = do
    echo <- getLine
    let lower = map toLower echo
    putStrLn lower
    
main = do
    strEchoLower
    chrEchoLower