type 'a my_lazy_aux =
    | Delayed of (unit -> 'a )
    | Const of 'a
    | Busy

type 'a my_lazy = 'a my_lazy_aux ref

let force x =
    match !x with
    | Const x   -> x
    | Busy -> failwith "err"
    | Delayed f ->
            x := Busy;
            let r = f () in
            x := Const r;
            r

let make_lazy x =
   ref (Delayed (fun () -> x))

let rec fix (f : ('a my_lazy -> 'a)) =
   ref (Delayed (fun () -> (f (fix f))))

type 'a llist = 
    | Nil
    | Lcons of 'a * ('a llist) my_lazy

let rec nats n =
    make_lazy(Lcons(n, nats (n+1)))

let primes =
    let div n x =
        if (x mod n == 0) then true else false in
    let rec filter xs p =
        match force xs with
        | Nil -> Nil
        | Lcons(hd, tl) ->
                if p hd then filter tl p else Lcons(hd, make_lazy(filter tl (div hd))) in

    (filter (make_lazy (Lcons(2, (nats 3)))) (div 2)) 

             (* let tl = force tl in
                match tl with
                    | Nil -> if p hd then Nil else Lcons(hd, Nil) 
                    | Lcons(hd, tl) -> if p hd then Lcons(hd, (filter (filter lls p) div_by_hd?)) else filter lls p in *)
