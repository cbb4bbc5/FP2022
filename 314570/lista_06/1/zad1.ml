type ('a, 'b) format = (string -> 'b) -> string -> 'a

let lit:(string -> ('a,'a) format) = 
  fun text cont prev_text -> cont (prev_text^text) 

let int:((int->'a,'a) format) =
  fun cont text int -> cont (text^(string_of_int int))

let str:((string -> 'a, 'a) format) =
  fun cont text new_text -> cont (text^new_text)


let (^^):(('a, 'b) format -> ('b, 'c) format -> ('a, 'c) format) = 
  fun p1 p2 cont -> p1 (p2 cont) 


let ksprintf:(('a, 'b) format -> (string -> 'b) -> 'a) = 
  fun format cont -> format cont ""


let sprintf:(('a, string) format -> 'a) =
  fun p -> ksprintf p (fun x -> x)
