
let rec fold_left_cps : ('a -> 'b -> ('a -> 'c) -> 'c) -> 'a -> 'b list -> ('a -> 'c) -> 'c=
    fun f a xs c -> match xs with
    | [] -> c a
    | x::xs -> f a x (fun a -> fold_left_cps f a xs c)

let fold_left f a xs = fold_left_cps (fun a b c -> f (c a) b) a xs (fun q -> q)
