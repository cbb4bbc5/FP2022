import Data.Char(toLower)

data StreamTrans i o a
  = Return a
  | ReadS (Maybe i -> StreamTrans i o a)
  | WriteS o (StreamTrans i o a)

toLower :: StreamTrans Char Char () 
toLower = ReadS (\r -> case r of 
												Nothing -> Return () 
												Just r -> WriteS (Data.Char.toLower r) Main.toLower)

listTrans :: StreamTrans i o a -> [i] -> ([o], a)
listTrans (Return a) xs = ([], a)
listTrans (ReadS f) xs = 
  case xs of
  [] -> listTrans (f Nothing) [] 
  x:xs -> listTrans (f $ Just x) xs  
listTrans (WriteS x c) xs =
  let (lst, ret) = listTrans c xs
  in (x:lst, ret)

main = putStrLn $ take 3 $ fst $ listTrans Main.toLower $ ['A'..]
