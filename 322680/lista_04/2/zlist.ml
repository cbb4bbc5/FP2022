type 'a zlist = 'a list * 'a list

let of_list xs = [], xs

let to_list (ctx, xs) = List.rev_append ctx xs

let elem (ctx, xs) = 
  match xs with
  | []     -> None
  | x :: _ -> Some x

let move_left (ctx, xs) =
  match ctx with
  | [] -> failwith "Can't move left from the list's head"
  | x :: ctx -> ctx, x :: xs

let move_right (ctx, xs) =
  match xs with
  | [] -> failwith "Can't move right from the list's end"
  | x :: xs -> x :: ctx, xs

let insert x (ctx, xs) = ctx, x :: xs

let remove (ctx, xs) =
  match xs with
  | [] -> failwith "Can't remove any element from an empty list"
  | x :: xs -> ctx, xs
