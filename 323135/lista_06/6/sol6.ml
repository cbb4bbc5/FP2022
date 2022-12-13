open Proc
let rec map (f : 'i -> 'o) : ('a, 'z, 'i, 'o) proc =
  fun k -> recv (fun v -> send (f v) (fun () -> map f k))

(* run (map String.length >|> map string_of_int) *)

let rec filter (pred : 'i -> bool) : ('a,'z, 'i, 'i) proc =
  fun k -> recv (fun v -> if pred v then send v (fun () -> filter pred k) else filter pred k)

(* run (filter (fun s -> String.length s >= 5)) *)

let rec nats_from (n : int) : ('a, 'z, 'i, int) proc =
  fun k -> send n (fun () -> nats_from (n + 1) k)

(* run (nats_from 2 >|> map string_of_int) *)

let rec sieve : ('a, 'a, int, int) proc =
  fun n -> recv (fun v -> send v (fun () -> (filter (fun x -> x mod v <> 0) >|> sieve) n))

(* run (nats_from 2 >|> sieve >|> map string_of_int) *)
