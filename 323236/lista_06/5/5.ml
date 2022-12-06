let rec fold_left_cps : ('a -> 'b -> ('a -> 'c) -> 'c) -> 'a -> 'b list -> ('a -> 'c) -> 'c =
  fun f init xs cont -> match xs with
  | []      -> cont init
  | x :: xs -> f init x (fun a -> fold_left_cps f a xs cont)


let for_all p xs =
  fold_left_cps
  (fun a b cont -> match (p b) with
  | false -> false
  | true  -> cont true)
  true
  xs
  (fun x -> x)

let mult_list xs =
  fold_left_cps
  (fun a b cont -> match b with
  | 0 -> 0
  | b -> cont (a*b))
  1
  xs
  (fun x -> x)

let sorted xs =
  if (xs == []) then true
  else fold_left_cps
  (fun a b cont -> match (a <= b) with
   | true  -> cont b
   | false -> false)
  (List.hd xs)
  (List.tl xs)
  (fun _ -> true)