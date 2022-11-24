
type ('a, 'b) format = (string -> 'b) -> (string -> 'a)

let lit : string -> ('a, 'a) format =
  fun s f a -> f (a^s)

let str : (string -> 'a, 'a) format =
  fun f a s -> lit s f a  

let int : (int -> 'a, 'a) format =
  fun f a i -> str f a (string_of_int i)

let (^^) : ('a, 'b) format -> ('b, 'c) format -> ('a, 'c) format =
  fun f g c -> f (g c)

let sprintf : ('a, string) format -> 'a =
  fun f -> f (fun x -> x) ""
