let length l = List.fold_left (fun a b -> a + 1) 0 l
let rev l = List.fold_left (fun a b -> b :: a) [] l
let map f l = List.fold_right (fun a b -> (f a) :: b) l []
let append a l = rev (a :: (rev l))
let append a l = List.fold_right (fun a b -> a :: b) l [a]
let rev_append a l = List.fold_left (fun a b -> b :: a) [a] l
let filter p l = List.fold_right (fun a b -> (if p a then a :: b else b)) l []
let rev_map f l = List.fold_left (fun a b -> (f b) :: a) [] l