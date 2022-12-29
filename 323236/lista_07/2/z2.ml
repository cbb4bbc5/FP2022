module type RandomMonad = sig
  type 'a t
  val return : 'a -> 'a t
  val bind : 'a t -> ('a -> 'b t) -> 'b t
  val random : int t
end

module RS : sig
  include RandomMonad

  val run : int -> 'a t -> 'a
end = struct
  type 'a t = int -> 'a * int

  let bind m f s = let (v, s) = (m s) in f v s

  let random = fun s ->
    let b = 16807 * (s mod 127773) - 2836 * (s / 127773) in
    let a = if b >= 0 then b else b + 2147483647 in
    (a, a)

  let return x = fun s -> (x, s)
  let run s m = fst (m s)
end

(*

#use "z1.ml";;
#use "z2.ml";;
module S = Shuffle(RS);;
S.shuffle [1; 2; 3; 4] |> RS.run 1337 ;;

*)