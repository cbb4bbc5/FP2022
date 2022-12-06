open Proc

let rec echo  =
fun k ->
  recv (fun v ->
  send v (fun () ->
  echo k))

let rec map =
fun f k ->
  recv (fun v ->
  send (f v) (fun () ->
  echo k))

let rec filter =
fun f k ->
  recv (fun v ->
  match (f v) with
  | true -> send (f v) (fun () -> filter f k)
  | false -> filter f k)

let rec nats_from n k =
fun n k ->
  send v (fun () -> nats_from n+1 k)
 
let rec sieve =
  fun k ->
    recv (fun x ->
      send x (fun () ->
      (filter (fun y -> not (y mod x == 0)) >|> sieve) k))


(* let _ = run (map String.length >|> map string_of_int) *)
(* let _ = run (filter (fun s -> String.length s >= 5)) *)
(* let _ = run (nats_from 2 >|> map string_of_int) *)
(* let _ = run (nats_from 2 >|> sieve >|> map string_of_int) *)
  
