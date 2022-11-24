open Proc
let rec map : ('i -> 'o) -> ('a, 'z, 'i, 'o) proc =
  fun f c -> recv (fun v -> send (f v) (fun () -> map f c))

let rec filter : ('i -> bool) -> ('a,'z, 'i, 'i) proc =
  fun f c -> recv (fun v -> if f v then send v (fun () -> filter f c) else filter f c)

let rec nats_from : int -> ('a, 'z, 'i, int) proc =
  fun n c -> send n (fun () -> nats_from (n+1) c)


let rec sieve : ('a, 'a, int, int) proc =
  fun c -> recv (fun v -> send v (fun () -> (filter (fun x -> x mod v <> 0) >|> sieve) c))

let primes = fun () -> run (nats_from 2 >|> sieve >|> map string_of_int)
