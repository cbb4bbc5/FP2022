let rec insert x xs i =
  match i, xs with
  | _, [] -> [x]
  | 0, xs -> x :: xs
  | i, h :: xs -> h :: insert x xs (i - 1)

let generate_permutations len x xs = List.init len (insert x xs)

let rec permutations_insert xs =
  match xs with
  | [] -> [[]]
  | x :: xs -> permutations_insert xs
               |> List.map (generate_permutations (List.length xs)  x)
               |> List.flatten

let rec select xs i =
  match i, xs with
  | _, []      -> failwith "Can't take element from empty list"
  | 0, x :: xs -> x, xs
  | i, x :: xs -> let x', xs' = select xs (i - 1) in x', x :: xs'

let rec permutations_select xs =
  match xs with
  | [] -> [[]]
  | _  -> List.init (List.length xs) (select xs)
          |> List.map (fun (x, xs) -> List.map (fun xs -> x :: xs) (permutations_select xs))
          |> List.flatten
