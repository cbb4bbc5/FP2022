(* Podstawowy kombinator *)
let rec fix f x = f (fix f) x

let fib_f fib n =
  if n <= 1 then n
  else fib (n-2) + fib (n-1)


let fib = fix fib_f

(* Kombinator z limitem *)
let rec fix_with_limit limit f x = 
  if limit <= 0 then failwith "Exceeded the limit of recursion"
  else f (fix_with_limit (limit - 1) f) x


(* Kombinator ze spamiętywaniem *)
let fix_memo f x =
     let rec fix_memo_hash hashtbl f x =
        match Hashtbl.find_opt hashtbl x with
        | Some fx -> fx
        | None -> let result = f (fix_memo_hash hashtbl f) x in 
                    let _ = Hashtbl.add hashtbl x result in
                        result 
     in fix_memo_hash (Hashtbl.create 10) f x

let fib = fix_memo fib_f

let _ = fib 10 |> string_of_int |> print_endline
let _ = fib 20 |> string_of_int |> print_endline
let _ = fib 30 |> string_of_int |> print_endline
let _ = fib 50 |> string_of_int |> print_endline
let _ = fib 80 |> string_of_int |> print_endline
let _ = fib 90 |> string_of_int |> print_endline
let _ = fib 100 |> string_of_int |> print_endline
