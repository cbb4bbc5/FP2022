(* Składnia *)
type (_, _) format = 
| Lit : string -> ('a, 'a lazy_t) format 
| Int : (int -> 'a, 'a lazy_t) format 
| Str : (string -> 'a, 'a lazy_t) format 
| Cat : ('a, 'b lazy_t) format * ('b, 'c) format -> ('a, 'c) format 

(* Semantyka *)
let rec ksprintf : type a b. (a, b) format -> (string -> b) -> a = 
  fun fmt ->
  match fmt with 
  | Lit s -> fun cont -> Lazy.force (cont s) 
  | Int -> fun cont n -> Lazy.force (cont (string_of_int n))
  | Str -> fun cont s -> Lazy.force (cont s) 
  | Cat(fmt1, fmt2) -> fun cont -> ksprintf fmt1 (fun s -> lazy (ksprintf fmt2 (fun x -> cont (s ^ x))))
;;

let sprintf fmt = ksprintf fmt (fun s -> lazy s)

let koty n = sprintf (Cat(Lit "Ala ma ", 
                          Cat(Int, 
                              Cat(Lit " kot",
                                  Cat(Str, Lit "."))))) 
                     n 
                     (if n = 1 then "a" else if 1 < n && n < 5 then "y" else "ów")
;;

let rec kprintf : type a b. (a, b) format -> b -> a = 
  fun fmt -> 
  match fmt with 
  | Lit s -> fun u -> print_string s ; Lazy.force u
  | Int -> fun u n -> string_of_int n |> print_string ; Lazy.force u
  | Str -> fun u s -> print_string s ; Lazy.force u
  | Cat(fmt1, fmt2) -> fun u -> kprintf fmt1 (lazy (kprintf fmt2 u))
;;

let printf fmt = kprintf fmt (lazy ());;
