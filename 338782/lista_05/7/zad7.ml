type 'a my_lazy =  (unit -> 'a) ref

let my_lazy : (unit -> 'a) -> 'a my_lazy = fun f -> let rec x = ref (fun () -> let v=f() in x:=(fun () ->v);v) in x

let force : 'a my_lazy -> 'a = fun l -> !l () 

let rec fix f = my_lazy (fun () -> f (fix f))

type 'a llist = 
    | Nil
    | Cons of 'a * 'a llist my_lazy




let st = fix (fun s -> Cons(1,s))

let hd ll = match force ll with
    | Nil -> failwith "empty list"
    | Cons(x,xs) -> x


let tl ll = match force ll with
    | Nil -> failwith "empty list"
    | Cons(x,xs) -> xs

let rec take_while pred ll = my_lazy begin fun () ->
    match force ll with
    | Nil -> Nil
    | Cons (x,xs) when not (pred x) -> Nil
    | Cons (x,xs) -> Cons(x,take_while pred xs)
end

let rec filter pred ll = my_lazy begin fun () ->
    match force ll with
    | Nil -> Nil
    | Cons(x,xs) -> if pred x then Cons(x,filter pred xs) else filter pred xs |> force
end

let rec for_all pred ll =
    match force ll with
    | Nil -> true
    | Cons(x,xs) -> pred x && for_all pred xs

let nats = let rec nexts n ll = Cons(n,my_lazy (fun () -> nexts (n+1) ll)) in fix (nexts 3)


let primes = let rec prim1 () = let prim = my_lazy prim1 in
    let is_prime n = take_while (fun x -> x*x <= n) prim |> for_all (fun x -> n mod x <> 0) in
        Cons(2,filter is_prime nats)
in my_lazy prim1


