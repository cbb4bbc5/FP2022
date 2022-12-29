
module type RandomMonad = sig
  type 'a t
  val return : 'a -> 'a t
  val bind : 'a t -> ('a -> 'b t) -> 'b t
  val random : int t
end

module Shuffle(R : RandomMonad) : sig
  val shuffle : 'a list -> 'a list R.t
end = struct
  let (let*) = R.bind
  let fmap f m = R.bind m (fun v ->  R.return (f v))

  let rec insert_at xs x n =
    match xs, n with
    | xs, 0       -> x :: xs
    | [], n       -> failwith "Out of bounds"
    | x' :: xs, n -> x' :: (insert_at xs x (n - 1))
      
  let shuffle xs =
    let rec aux xs =
      match xs with
      | []      -> R.return ([], 0)
      | x :: xs ->
        let* (xs, n) = aux xs in
        let* rand_id = R.random in
        let xs = insert_at xs x (rand_id mod (n + 1)) in
        R.return (xs, n + 1)
    in
      aux xs |> fmap fst


    
end
