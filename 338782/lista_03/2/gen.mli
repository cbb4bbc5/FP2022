
module type S = 
    sig
        type t
        val is_generated : t -> t list -> bool
    end

module Make(Elt : Perm.S) : S with type t = Elt.t

val is_generated : (module Perm.S with type t = 'a) -> 'a -> 'a list -> bool
