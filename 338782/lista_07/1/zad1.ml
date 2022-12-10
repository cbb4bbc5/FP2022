module type RandomMod = sig
    type 'a t
    val return : 'a -> 'a t
    val bind : 'a t -> ('a -> 'b t) -> 'b t
    val random : int t
end

module Shuffle(R : RandomMod) : sig
    val shuffle : 'a list -> 'a list R.t 
end = struct
    let ( let* ) = R.bind 

    let rec insert n i xs = match (n,xs) with
        | (0,_) -> i::xs
        | (_,x::xs) -> x::insert (n-1) i xs
        | _ -> raise (Invalid_argument "Index out of bounds")
    let rec shuffle = function
            | [] -> R.return []
            | x::xs -> 
                let len = List.length xs + 1 in
                let* rand = R.random in
                let* xs = shuffle xs in
            R.return (insert (rand mod len) x xs)
end
