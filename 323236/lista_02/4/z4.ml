open List;;

let rec merge cmp xs ys =
  match xs, ys with
  | xs, [] | [], xs    -> xs
  | x :: xs', y :: ys' ->
    if cmp x y
    then x :: merge cmp xs' ys
    else y :: merge cmp ys' xs

let tmerge cmp xs ys =
  let rec aux xs ys acc = match xs, ys with
  | [], []             -> rev acc
  | x :: xs, []
  | [], x :: xs        -> aux xs [] (x :: acc)
  | x :: xs', y :: ys' ->
      if cmp x y
      then aux xs' ys (x :: acc)
      else aux ys' xs (y :: acc)
  in
  aux xs ys []
    

let halve xs =
  let rec f xs ts acc =
    match ts with
    | [] | [_]  -> acc, xs
    | _ :: _ :: ts -> f (tl xs) ts (hd xs :: acc)
  in f xs xs []

let rec mergesort s = 
  match s with
  | [] | [_] -> s
  | xs ->
    let (ls, rs) = halve xs
    in tmerge (<=) (mergesort ls) (mergesort rs)
