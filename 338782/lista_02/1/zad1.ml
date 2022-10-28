let length xs = List.fold_left (fun a b -> a+1) 0 xs
let rev xs = List.fold_left (fun xs x -> x :: xs) [] xs
let map f xs = List.fold_right (fun x xs -> f x :: xs) xs []
let rev_map f = List.fold_left (fun xs x -> f x :: xs) []
let append x xs = List.fold_right List.cons x xs
let rev_append x xs = List.fold_left (fun xs x -> x::xs) xs x
let filter f xs = List.fold_right (fun x xs -> if f x then x::xs else xs) xs []
