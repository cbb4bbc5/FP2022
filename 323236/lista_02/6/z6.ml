open List;;

let rec insert x xs n =
  match n, xs with
  | 0, xs      -> x :: xs
  | _, []      -> [x]
  | n, y :: xs -> y :: insert x xs (n - 1);;

let extend_perms n x xs = init n (insert x xs);;
let rec permutations xs =
  let len = length xs in
  match xs with
  | [] -> [[]]
  | x :: xs -> flatten (map (extend_perms len x) (permutations xs));;

