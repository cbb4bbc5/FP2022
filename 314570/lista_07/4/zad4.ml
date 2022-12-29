module type Monad = sig
    type 'a t
    val return  : 'a -> 'a t
    val bind    : 'a t -> ('a -> 'b t) -> 'b t
end
module Err : sig
    include Monad
    val fail    : 'a t
    val catch   : 'a t -> (unit -> 'a t) -> 'a t
    val run     : 'a t -> 'a option
end
=
struct
    type 'r ans = 'r option
    type 'a t = { run : 'r. ('a -> 'r ans) -> 'r ans}

    let return x = { run = fun cont -> cont x } 
    let bind m t = { run = fun cont -> m.run (fun r -> (t r).run cont) }

    let fail = { run = fun cont -> None }

    let catch m f = 
        { run = fun cont -> 
            let ans = m.run cont in
                match ans with
                | None -> (f ()).run cont
                | Some _ -> ans 
        }

    let run m = m.run (fun x -> Some x)
end

module BT : sig
    include Monad
    val fail : 'a t
    val flip : bool t
    val run  : 'a t -> 'a Seq.t
end
=
struct
    type 'r ans = 'r Seq.t 
    type 'a t = { run : 'r. ('a -> 'r ans) -> 'r ans}
    let return x = { run = fun cont -> cont x } 
    let bind m t = { run = fun cont -> m.run (fun r -> (m r).run cont) }
    let fail = { run = fun cont -> Seq.empty }
    let flip = { run = fun cont -> Seq.append (cont true) (cont false) }
    let run m = m.run Seq.return
end

module St(State : sig type t end) : sig
    type 'a t

    val return : 'a -> 'a t
    val bind   : 'a t -> ('a -> 'b t) -> 'b t

    val get : State.t t
    val set : State.t -> unit t

    val run : State.t -> 'a t -> 'a
end
=
struct
    type 'r ans = State.t -> 'r * State.t 
    type 'a t = { run : 'r. ('a -> 'r ans) -> 'r ans
    
    let return x s = { run = fun cont -> cont (x, s) } 
    let bind m f s = { run = fun cont -> m.run s 

    let get s = { run = fun cont -> (cont s) s } 
    let set s _ = { run = fun cont -> (cont ()) s }
    let run s m = m.run (fun x _ -> x) s 
end
