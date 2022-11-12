
type 'a zlist = 'a list * 'a list

let of_list xs = [],xs

let to_list (l,r) = List.rev_append l r

let elem = function
  | (_,x::_) -> Some x
  | (_,[]) -> None

let move_left = function
  | (x::xs,r) -> (xs,x::r)
  | ([],_) -> failwith "Left of left"

let move_right = function
  | (l,x::xs) -> (x::l,xs)
  | (l,[]) -> failwith "Right of right"

let insert x (l,r) = (x::l,r)

let remove = function
  | (x::xs,r) -> xs,r
  | ([],_) -> failwith "Delete of left"
