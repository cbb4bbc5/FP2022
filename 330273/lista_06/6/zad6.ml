let rec echo k =
    recv (fun v -> send v (fun () -> echo k))

let rec map f k =
    recv (fun v -> send (f v) (fun () -> map f k))

let rec filter p k =
    recv (fun v ->
        if p v then
            send v (fun () -> filter p k) else
            filter p k)

let rec nats_from n k =
    send (string_of_int n) (fun () -> nats_from (n+1) k)

let rec sieve k =
    recv (fun n -> send n (fun () -> (filter (fun m -> m mod n <> 0) >|> sieve) k))
