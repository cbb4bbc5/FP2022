import StreamTrans

runCycle :: StreamTrans a a b -> b
runCycle st = let (os,x) = listTrans st os in x
