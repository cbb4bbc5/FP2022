
import StreamTrans

listTrans :: StreamTrans i o a -> [i] -> ([o],a)
listTrans (Return x) _ = ([],x)
listTrans (ReadS f) [] = listTrans (f Nothing) []
listTrans (ReadS f) (x:xs) = listTrans (f (Just x)) xs
listTrans (WriteS o st) xs = 
    let (os,a) = listTrans st xs in
        (o:os,a)

