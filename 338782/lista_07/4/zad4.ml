module type Monad = sig
    type 'a t
    val return : 'a -> 'a t
    val bind : 'a t -> ('a -> 'b t) -> 'b t
end
module Err : sig
    include Monad
    val fail : 'a t
    val catch : 'a t -> (unit -> 'a t) -> 'a t
    val run : 'a t -> 'a option
end = struct
    type 'a t = {run : 'r. ('a -> 'r option) -> 'r option}
    let return x ={run = fun c -> c x}
    let bind m f = {run = fun c -> m.run (fun x -> (f x).run c)}
    let fail = {run = fun c -> None}
    let catch m f = 
    {run = 
        fun c -> 
            match m.run c with
                | None -> (f ()).run c
                | x -> x
    }
    let run m = m.run (fun x -> Some x)
end
module BT : sig
    include Monad
    val fail : 'a t
    val flip : bool t
    val run : 'a t -> 'a Seq.t
end = struct
    type 'a t = {run : 'r. ('a -> 'r Seq.t) -> 'r Seq.t}
    let return x = {run = fun c -> c x}
    let bind m f = {run = fun c -> m.run (fun x -> (f x).run c)}
    let fail = {run = fun c -> Seq.empty}
    let flip = {run = fun c -> Seq.append (c true) (c false)}
    let run m = m.run (fun x -> Seq.cons x Seq.empty)
end
module St(State : sig type t end) : sig
    include Monad
    val get : State.t t
    val set : State.t -> unit t
    val run : State.t -> 'a t -> 'a
end = struct
    type 'a t = {run : 'r.  ('a -> State.t -> (State.t * 'r)) -> State.t -> (State.t * 'r) }
    let return x = {run = fun c s -> c x s}
    let bind m f = {run = fun c -> m.run (fun x -> (f x).run c)}
    let get = {run = fun c s -> c s s} 
    let set s = {run = fun c _ -> c () s}
    let run s m = snd (m.run (fun x s -> (s,x)) s)
end
