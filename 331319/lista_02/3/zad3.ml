let rec suffixes xs = 
  match xs with
  | [] -> [[]]
  | y :: ys -> xs :: (suffixes ys)
;;



let prefixes xs = 
  let rec go xs acc =
    match xs with 
    | [] -> [] :: acc
    | y :: ys -> go ys (xs :: acc)
  in go xs []
;;

