module type RandomMonad = sig
  type 'a t
  val return : 'a -> 'a t
  val bind : 'a t -> ('a -> 'b t) -> 'b t
  val random : int t
  val run : int -> 'a t -> 'a
end


module Shuffle(R : RandomMonad) : sig
    val shuffle : 'a list -> 'a list R.t 
end = struct
    let (let* ) = R.bind 

    let rec shuf_aux pos vl l =
      if pos = 0
         then vl::l
      else
        match l with
        | x::xs -> x::(shuf_aux (pos - 1) vl xs)
        | _ -> failwith "Incorrect position"

    let rec shuffle l = match l with
            | [] -> R.return []
            | x::xs -> 
                let* r = R.random in
                let* xs = shuffle xs in
            R.return (shuf_aux (r mod (List.length xs + 1)) x xs)
end


module RS : RandomMonad = struct
  type 'a t = int -> ('a * int)
  let gen a_i =
    let d = 127773 in
    let m1 = 16807 in
    let m2 = 2836 in
    let p = int_of_float (2.**31.) - 1 in
    let b_i = m1 * (a_i mod d) - m2 * (a_i / d) in
    if b_i > 0 then b_i else b_i + p

  let return x s = (x, gen s)

  let bind m fn s =
    let (x, a) = m (gen s) in
    let (y, b) = fn x (gen a) in (y, gen b)

  let random s = (gen s, gen s)

  let run s m = m s |> fst
end

module Mytest = Shuffle(RS)
let x = Mytest.shuffle (List.init 10 Int.succ)
RS.run 29 x
