let length l = List.fold_left (fun x y -> x + 1) 0 l;;
let rev l = List.fold_left (fun x y -> y :: x) [] l;;
let map f l = List.fold_right (fun y x -> (f y)::x) l [];;
let append l elem = List.fold_right (fun y x -> y::x) l [elem];;
let rev_append l1 l2 = List.fold_left (fun x y -> y::x) l2 l1;;
let filter f l = List.fold_right (fun x y -> if f x then x::y else y) l [];;
let rev_map f l = List.fold_left (fun x y -> (f y) :: x) [] l;;
