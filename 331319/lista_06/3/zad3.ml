exception ResultKnown

let for_all p xs = try List.fold_left 
  (fun acc x -> 
    if p x then true else raise ResultKnown) 
  true xs
  with ResultKnown -> false
;;

let mult_list xs = try List.fold_left 
  (fun acc x -> 
    if x <> 0 then x*acc else raise ResultKnown)
  1 xs 
  with ResultKnown -> 0
;;

let sorted = function
  | [] -> true 
  | x :: xs -> 
    try fst (List.fold_left 
              (fun acc x -> 
                if snd acc <= x then (true, x) else raise ResultKnown)
              (true, x) 
              xs)
    with ResultKnown -> false 
;;
