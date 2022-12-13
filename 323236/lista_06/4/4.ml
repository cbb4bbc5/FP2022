let rec fold_left_cps : ('a -> 'b -> ('a -> 'c) -> 'c) -> 'a -> 'b list -> ('a -> 'c) -> 'c =
  fun f acc xs cont -> match xs with
  | []      -> cont acc
  | x :: xs -> f acc x (fun a -> fold_left_cps f a xs cont)


let fold_left : ('a -> 'b -> 'a) -> 'a -> 'b list -> 'a =
  fun fold acc xs ->
    fold_left_cps (fun a b cont -> cont (fold a b)) acc xs (fun x -> x)
    