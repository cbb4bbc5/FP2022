module IdentityMonad = struct
    type 'a t = 'a
    let return x = x
    let bind x f = f x
end

module UnitMonad = struct
    type 'a t = unit -> 'a
    let return x () = x
    let bind x f () = f (x ()) ()
end
