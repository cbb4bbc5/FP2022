let rec fix f x = f (fix f) x

exception TooDeep of string

let rec fix_with_limit lim f x = 
  if lim = 0 then raise (TooDeep "Uh oh!")
  else f (fix_with_limit (lim - 1) f) x
;;

let fib_f fib n = 
  if n <= 1 then n 
  else fib (n - 1) + fib (n - 2)
;;

let rec fix_with_hash htb f x = 
  match Hashtbl.find_opt htb x with 
  | Some y -> y 
  | None -> let y = f (fix_with_hash htb f) x in
    Hashtbl.add htb x y ; y
;;

let fix_memo f x = fix_with_hash (Hashtbl.create 20) f x
