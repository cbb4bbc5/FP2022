let length xs = List.fold_right (fun x y -> (1 + y)) xs 0
let rev xs    = List.fold_left (fun x y -> y :: x) [] xs
let append xs ys = List.fold_right (fun x y -> x :: y) xs ys
let map xs f = List.fold_right (fun x y -> (f x) :: y) xs []
let map_rev xs ys = List.fold_left (fun x y -> y :: x) ys xs
let filter xs f = List.fold_right (fun x y -> if (f x) then x :: y else y) xs []
let rev_map xs f = List.fold_left (fun x y -> (f y) :: x) [] xs
