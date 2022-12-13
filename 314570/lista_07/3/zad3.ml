module type Monad = sig 
    type 'a t
    val return : 'a -> 'a t
    val bind : 'a t -> ('a -> 'b t) -> 'b t
end


module IdMonad : sig 
    include Monad 
    val get : 'a t -> 'a
end 
= 
struct
    type 'a t = 'a
    let return x = x
    let bind x f = f x 
    let get x = x
end

module DerefferedMonad : sig
    include Monad 
    val run : 'a t -> 'a
end 
=
struct
    type 'a t = unit -> 'a
    let return x = fun () -> x 
    let bind x f = f (x ()) ()
    let run x = x ()
end
    
