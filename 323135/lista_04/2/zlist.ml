type 'a zlist = 'a list * 'a list

let of_list xs = [], xs

let to_list (l, r) = List.rev_append l r

let elem ls = match ls with
  | (_, x::_) -> Some x
  | (_, []) -> None

let move_left ls = match ls with
  | (x::xs, r) -> (xs, x::r)
  | ([], _) -> failwith "move_left failed"

let move_right ls = match ls with
  | (l, x::xs) -> (x::l, xs)
  | (l, []) -> failwith "move_right failed"

let insert x (l, r) = (x::l, r)

let remove ls = match ls with
  | (x::xs, r) -> xs, r
  | ([], _) -> failwith "Cannot remove"
