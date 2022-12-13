type ('a, 'b) format =
  | Lit : string -> ('a, 'a) format
  | Int : (int -> 'a, 'a) format
  | Str : (string -> 'a, 'a) format
  | Cat : ('a, 'b) format * ('b, 'c) format -> ('a, 'c) format


let rec kprintf : type a b. (a, b) format -> b -> a =
  fun fmt x -> match fmt with
  | Lit s -> print_string s; x
  | Str -> fun s -> print_string s; x
  | Int -> fun i -> print_string (string_of_int i); x
  | Cat(Str, g) -> fun s -> print_string s; kprintf g x
  | Cat(Int, g) -> fun i -> print_string (string_of_int i); kprintf g x
  | Cat(Cat(cl, cr), g) -> kprintf (Cat(cl, Cat(cr, g))) x
  | Cat(f, g) -> kprintf f (kprintf g x)


let printf : ('a, 'b) format -> 'a =
  fun f -> kprintf f ()


let rec ksprintf : type a b. (a, b) format -> (string -> b) -> a = 
  fun con -> match con with
  | Lit s -> fun f -> f s
  | Str -> fun f s -> f s
  | Int -> fun f i -> f (string_of_int i)
  | Cat(a, b) -> fun f -> ksprintf a (fun s -> ksprintf b (fun x -> f (s ^ x)))


let id x = x

let sprintf : ('a, string) format -> 'a =
  fun f -> ksprintf f id

(* printf *)
(* printf (Lit "affdsaf");; *)


(* sprintf *)
(* sprintf Cat(Lit "a", Lit "b");; *)
(* sprintf (Cat(Int, Lit " a")) 5;; *)
(* sprintf (Cat(Int, Str)) 5 " 29" *)
(* sprintf (Cat(Cat(Int, Str), (Cat(Lit " test ", Int))));; *)
(* sprintf (Cat(Cat(Int, Str), (Cat(Lit " test ", Int)))) 353 "a" 29;; *)
