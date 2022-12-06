let mult_list l = 
    match (List.fold_left (fun x y -> if x * y = 0 then failwith "0" else x * y) 1 l) with
    | exception _ -> 0
    | x -> x


let for_all f l =
  match (List.fold_left (fun x y -> if x && (f y) then x && (f y) else failwith "False") true l) with
  | exception _ -> false
  | _ -> true


let sorted l =
  if l = []
  then true
  else
  match (List.fold_left (fun x y -> if x < y then y else failwith "Not sorted") (List.hd l) (List.tl l)) with
  | exception _ -> false
  | _  -> true
