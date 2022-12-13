open Proc
let rec map:(('i -> 'o) -> ('a, 'z, 'i, 'o) proc) = 
  fun f k -> recv (fun r -> 
               send (f r) (fun () -> map f k))


let rec filter:(('i -> bool) -> ('a, 'z, 'i, 'i) proc) = 
  fun f k -> recv (fun r -> if f r then
                  send r (fun () -> filter f k)
                  else filter f k)

let rec nats_from:(int -> ('a, 'z, 'i, int) proc) =
  fun n k -> send n (fun () -> nats_from (n+1) k)


let rec sieve k =
  recv (fun r -> send r (fun () -> (filter (fun x -> x mod r != 0) >|> sieve) k))


