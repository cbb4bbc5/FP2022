open List;;

let rec suffixes s =
  match s with  
  | [] -> [[]]
  | (hd :: tl) -> s :: suffixes tl;;

let prefixes xs = 
  let rec go xs acc =
    match xs with 
    | [] -> [[]] :: acc
    | _ :: ys -> go ys (xs :: acc)
  in go xs [];;