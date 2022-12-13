let rec fold_left_cps f b xs cont =
    match xs with
    | [] -> cont b
    | h::tl -> f h b (fun v -> (fold_left_cps f v tl cont)) 

let fold_left f b xs = fold_left_cps (fun x xs cont -> f (cont xs) x) b xs (fun x -> x)

let for_all p xs =
    fold_left_cps (fun x y cont -> 
        if p x then cont true
        else false) true xs (fun x -> x)

let nats k =
    let rec aux n =
        if n == k then []
        else n::(aux (n+1)) in
    aux 0

let mult_list xs =
    fold_left_cps (fun x y cont -> 
        if x * y = 0 then 0
        else cont (x * y)) 1 xs (fun x -> x)

let sorted xs =
    let r = ref Int.min_int in
    fold_left_cps (fun x y cont ->
        if !r <= y then (r := y; cont true) else false) true xs (fun x -> x)   
