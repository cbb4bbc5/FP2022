type ('a, 'b) format = (string -> 'b) -> (string -> 'a)

let lit : string -> ('a, 'a) format = fun s fn a -> fn (a ^ s)

let str : (string -> 'a, 'a) format = fun fn a s -> lit s fn a  

let int : (int -> 'a, 'a) format =
  fun fn a i -> let si = string_of_int i in str fn a si

let (^^) = fun f g c -> f (g c)

let id x = x

let sprintf : ('a, string) format -> 'a = fun f -> f id ""

let sp1 n = sprintf (lit "Ala ma " ^^ int ^^ lit " kot" ^^ str ^^ lit ".") n
(if n = 1 then "a" else if 1 < n && n < 5 then "y" else "ów")
