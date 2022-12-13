exception Finished of string;;


let for_all p xs =
  let my_pred x = 
    match (p x) with
    | true -> true
    | false -> raise (Finished "false")
  in try List.fold_left (fun a b -> my_pred b && a) true xs
with Finished _ -> false;;

let mult_list xs = 
  let checker acc x =
    match x with
    | 0 -> raise (Finished "zero")
    | non_zero -> non_zero * acc
  in try List.fold_left (checker) 1 xs
with Finished _ -> 0;;


let comp : ((bool * int) -> int -> (bool * int)) = fun (res, prev) x -> 
  match (prev <= x) with
  | false -> raise (Finished "not sorted")
  | true -> (true, x) 

let sorted xs =
if xs == [] then true
  else try (fst (List.fold_left 
  (comp)
  (true, List.hd xs)
  List.tl xs))
with Finished _ -> false;;
  