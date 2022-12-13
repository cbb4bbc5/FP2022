type ('a, 'b) format =
  | Lit : string -> ('a, 'a) format
  | Int : (int -> 'a, 'a) format
  | Str : (string -> 'a, 'a) format
  | Cat : ('a, 'b) format * ('b, 'c) format -> ('a, 'c) format

let rec ksprintf : type a b. (a, b) format -> (string -> b) -> a = function
  | Lit s -> fun f -> f s
  | Str -> fun f s -> f s
  | Int -> fun f i -> f (string_of_int i)
  | Cat(a,b) -> fun f -> ksprintf a (fun s -> ksprintf b (fun x -> f (s ^ x)))

let sprintf : ('a, string) format -> 'a =
  fun f -> ksprintf f (fun s -> s)

let rec kprintf : type a b. (a, b) format -> b -> a = fun form b -> match form with
  | Lit s -> print_string s;b
  | Str -> fun s -> print_string s;b
  | Int -> fun i -> print_string (string_of_int i);b
  | Cat(Str,g) -> fun s -> print_string s;kprintf g b
  | Cat(Int,g) -> fun i -> print_string (string_of_int i);kprintf g b
  | Cat(Cat(f1,f2),g) -> kprintf (Cat(f1,Cat(f2,g))) b
  | Cat(f,g) -> kprintf f (kprintf g b)

let printf : ('a, unit) format -> 'a =
  fun f -> kprintf f ()
