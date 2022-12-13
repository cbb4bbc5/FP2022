type ('a, 'b) format = (string -> 'b) -> string -> 'a

let lit (str : string) =
    (fun (k : (string -> 'a)) -> (fun arg -> (k (arg ^ str))))

let int k =
    (fun str -> (fun n -> k (str ^ (string_of_int n))))

let str k =
    (fun str -> (fun arg -> k (str ^ arg)))

let (^^) f g =
    (fun k -> f (g k))

let sprintf (k : ('a, string) format) =
    (k (fun x -> x ^ "")) ""
    
