type 'a zlist =
    | Zlist of 'a list * 'a list

let of_list xs =
    Zlist([], xs)

let to_list zs =
    match zs with
    | Zlist(xs, ys) -> List.append (List.rev xs) ys

let elem zs =
    match zs with
    | Zlist(xs, []) -> None
    | Zlist(xs, t::ys) -> Some(t, Zlist(xs, ys))

let move_left zs =
    match zs with
    | Zlist([], ys) -> failwith "err"
    | Zlist(t::xs, ys) -> Zlist(xs, t::ys)

let move_right zs =
    match zs with
    | Zlist(xs, []) -> failwith "err"
    | Zlist(xs, t::ys) -> Zlist(t::xs, ys)

let insert a zs =
    match zs with
    | Zlist(xs, ys) -> Zlist(xs, a::ys)

let remove zs =
    match zs with
    | Zlist([], ys) -> failwith "err"
    | Zlist(t::xs, ys) -> Zlist(xs, ys)
    
