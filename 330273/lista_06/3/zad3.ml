exception Zero

let my_check p x =
    if p x then p x else raise Exit

let for_all p xs =
    let t = List.fold_left (fun x y -> my_check p y) true xs in
    try t with
    | Exit -> false

let my_mult x y =
    if x * y = 0 then raise Zero else x * y

(*  (fun x y -> try my_mult x y with | Zero -> 0) *)

let mult_list xs =
    let t = List.fold_left my_mult 1 xs in
    try t with
    | Zero -> 0

let sorted xs =
    let r = ref Int.min_int in
    try List.fold_left (fun x y -> if !r <= y then (r := y; true) else raise Exit) true xs with
        | Exit -> false

let nats =
    let rec aux n =
        if n = 100000 then []
        else n::(aux (n+1)) in
    aux 0
