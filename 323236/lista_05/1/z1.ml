let rec fix f x = f (fix f) x;;


let fib_f fib n =
  if n <= 1 then n
  else fib (n-1) + fib (n-2);;

let fib = fix fib_f;;

exception Fix_exception of string;;

let rec fix_with_limit n f x = 
  if (n > 0) then f (fix_with_limit (n-1) f) x
  else raise (Fix_exception "depth limit exceeded");;


let fib_limit = fix_with_limit 3 fib_f;;
 
let rec fix_memo f x =
  let f_memo = Hashtbl.create 0
  in let rec aux f x =
    match Hashtbl.find_opt f_memo x with
    | Some(res) -> res
    | None      -> let res = f (aux f) x
                    in Hashtbl.add f_memo x res; res;
  in aux f x;;

let fib_memo = fix_memo fib_f;;