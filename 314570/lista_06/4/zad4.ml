let rec fold_left f acc xs =
  match xs with
  | [] -> acc
  | x::xs -> fold_left f (f acc x) xs

let rec fact n k = 
  match n with
  | 1 -> k 1
  | n -> fact (n-1) (fun r -> k (n * r)) 

let rec fact n k acc =
  match n with
  | 1 -> k acc
  | n -> k (fact (n-1) k (n * acc)) 


let rec fold_left_cps f acc xs k =
  match xs with
  | [] -> k acc
  | x::xs -> f acc x (fun r -> fold_left_cps f r xs k)

let rec fold_right_cps f acc xs k =
  match xs with
  | [] -> k acc
  | x::xs -> fold_right_cps f acc xs (fun r -> k (f x r))

let fold_left f acc xs = fold_left_cps (fun acc x cont -> cont (f acc x)) acc xs (fun x -> x)
let fold_right f xs acc = fold_right_cps f xs acc (fun x -> x)


(* fold_left_cps :
(’a -> ’b -> (’a -> ’c) -> ’c) -> ’a -> ’b list -> (’a -> ’c) -> ’c. *)

