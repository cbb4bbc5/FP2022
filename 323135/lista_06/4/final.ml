let rec fold_left_cps (f : 'a -> 'b -> ('a -> 'c) -> 'c) (acc : 'a) (l : 'b list) (k : 'a -> 'c) : 'c =
    match l with
    | [] -> k acc
    | x::xs -> f acc x (fun a -> fold_left_cps f a xs k)

let id x = x

let fold_left (f : 'a -> 'b -> 'a) (acc : 'a) (l : 'b list) : 'a =
    fold_left_cps (fun a b k -> f (k a) b) acc l id
