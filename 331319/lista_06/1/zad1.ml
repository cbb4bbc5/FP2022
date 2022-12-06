type ('a, 'b) format = (string -> 'b) -> string -> 'a

let lit s1 cont s2 = cont (s2 ^ s1) 

let int cont s n = cont (s ^ string_of_int n)

let str cont s1 s2 = cont (s1 ^ s2) 

let (^^) fmt1 fmt2 cont = fmt1 (fmt2 cont) 

let sprintf fmt = fmt (fun s -> s) ""
