open List;;

let tmerge cmp xs ys =
  let rec tailer xs ys acc = match xs, ys with
  | [], []                           -> acc
  | x :: xs, [] | [], x :: xs        -> tailer xs [] (x :: acc)
  | x :: xs', y :: ys' ->
    if cmp x y
    then tailer xs' ys (x :: acc)
    else tailer ys' xs (y :: acc)
  in tailer xs ys []

let halve xs =
  let rec f xs ts acc =
    match ts with
    | [] | [_]  -> acc, xs
    | _ :: _ :: ts -> f (tl xs) ts (hd xs :: acc)
  in f xs xs []

let mergesort s1 = 
  let rec flipmergesort s reversed =
  match s with
  | [] | [_] -> s
  | xs ->  let (ls, rs) = halve xs
           in tmerge (if reversed then (>) else (<=)) (flipmergesort ls (not reversed)) (flipmergesort rs (not reversed))
  in flipmergesort s1 true;;
