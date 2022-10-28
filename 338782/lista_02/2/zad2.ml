let map_append f x xs = List.fold_right (fun x xs -> f x :: xs) x xs
let rec sublists = function 
    | [] -> [[]]
    | (x::xs) -> let xs = sublists xs in map_append (List.cons x) xs xs
