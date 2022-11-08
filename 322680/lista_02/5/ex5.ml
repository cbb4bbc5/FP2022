let merge (cmp: 'a -> 'a -> bool) (l1: 'a list) (l2: 'a list) =
  let rec aux acc l1 l2 =
    match l1, l2 with
    | [], [] -> acc
    | h :: tail, [] | [], h :: tail -> aux (h :: acc) tail []
    | (h1 :: tail), (h2 :: _) when (cmp h1 h2) -> aux (h1 :: acc) tail l2
    | (h1 :: _), (h2 :: tail) -> aux (h2 :: acc) l1 tail
  in aux [] l1 l2

let halve (l: 'a list) =
  let rec aux acc l1 l2 =
    match l2 with
    | [] | [_] -> acc, l1
    | _ :: _ :: tail -> aux ((List.hd l1) :: acc) (List.tl l1) tail
  in aux [] l l

let rec mergesort (cmp: 'a -> 'a -> bool) (l: 'a list) =
  let l, r = halve l in let l, r = (mergesort cmp l), (mergesort cmp r)
  in merge cmp l r

let mergesort cmp l =
  let rec aux l inv =
    match l with
    | [] | [_] -> l
    | l ->
        let l, r = halve l
        in merge (if inv then (fun x y -> not (cmp x y)) else cmp) (aux l (not inv)) (aux r (not inv))
  in aux l true
