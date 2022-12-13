module type Monad = sig
  type 'a t
  val return : 'a -> 'a t
  val bind : 'a t -> ('a -> 'b t) -> 'b t
end

module IDMonad : sig
  include Monad
end = struct
  type 'a t = 'a
  let return a = a
  let bind a f = f a
end

module LMonad : sig
  include Monad
end = struct
  type 'a t = unit -> 'a
  let return a = fun () -> a
  let bind a f = fun () -> f (a ()) ()
end