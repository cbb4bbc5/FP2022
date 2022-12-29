let rec fix f x = f (fix f) x

let fib_f fib n =
  if n <= 1 then n
  else fib (n - 1) + fib (n - 2)

let fib = fix fib_f

let rec fix_with_limit : int -> (('a -> 'b) -> 'a -> 'b) -> 'a -> 'b =
  fun dl f x ->
    if dl < 1 then failwith "max depth exceeded"
    else f (fix_with_limit (dl - 1) f) x

let fix_memo : (('a -> 'b) -> 'a -> 'b) -> 'a -> 'b =
  let memo = Hashtbl.create 1000 in
  let rec aux f x =
    match Hashtbl.find_opt memo x with
    | None   -> let v = f (aux f) x in Hashtbl.add memo x v; v
    | Some v -> v
  in aux
