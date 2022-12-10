import StreamTrans

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

