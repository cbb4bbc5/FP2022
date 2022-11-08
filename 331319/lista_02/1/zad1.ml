(* Przypomnienie foldów 
  let rec foldl f s xs = 
    match xs with
    | [] -> s
    | x :: xs -> foldl f (f s x) xs
  
  let rec foldr f xs s =
    match xs with
    | [] -> s
    | x :: xs -> f x (foldr f xs s)
*)

let length xs =
  List.fold_left (fun x xs -> x + 1) 0 xs 
;;

let rev xs = 
  List.fold_left (fun xs x -> x :: xs) [] xs
;;

let map f xs =
  List.fold_right (fun x xs -> (f x) :: xs) xs []
;;

let append xs ys = 
  List.fold_right (fun x xs -> a :: b) xs ys
;;

(* Odwraca xs i doczepia ys *)
let rev_append xs ys = 
  List.fold_left (fun xs x ->
    if xs = [] then x :: ys
    else x :: xs) [] xs
  ;;
;;

let filter p xs = 
  List.fold_right (fun x xs ->
    if p x then x :: xs
    else xs) xs []
  ;;
;;

let rev_map f xs = 
  List.fold_left (fun xs x -> (f x) :: xs) [] xs
;;
