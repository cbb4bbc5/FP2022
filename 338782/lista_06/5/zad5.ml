
let rec fold_left_cps : ('a -> 'b -> ('a -> 'c) -> 'c) -> 'a -> 'b list -> ('a -> 'c) -> 'c=
    fun f a xs c -> match xs with
    | [] -> c a
    | x::xs -> f a x (fun a -> fold_left_cps f a xs c)

let fold_left f a xs = fold_left_cps (fun () b c -> f (c ()) b) () xs (fun () -> a)

let for_all f xs = fold_left_cps (fun () x c -> f x && c ()) () xs (fun () -> true)
let mult_list xs = fold_left_cps (fun () x c -> x * c ()) () xs (fun () -> 1)
let sorted xs = snd (fold_left_cps (fun () x1 c -> (Some x1,fun () -> match c() with (None,c) -> c() | (Some x2,c) -> x1<=x2 && c())) () xs (fun () -> (None,fun () -> true))) ()
