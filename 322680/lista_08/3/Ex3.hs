module Ex3 where

listTrans :: StreamTrans i o a -> [i] -> ([o], a)
listTrans (Return a) _        = ([], a)
listTrans (ReadS cont) []     = listTrans (cont Nothing) []
listTrans (ReadS cont) (x:xs) = listTrans (cont $ Just x) xs
listTrans (WriteS o s) xs =
  let (ys, a) = listTrans s xs
  in (o:ys, a)

