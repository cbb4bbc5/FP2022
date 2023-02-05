type rational = int * int

type qtree =
    | Qtree of (unit -> qtree) * rational * (unit -> qtree)

let all_rationals =
    let rec construct q =
        match q with
        | ((p1, q1), (p2, q2)) -> 
                let p3 = p1 + p2 and q3 = q1 + q2 in
                fun () -> Qtree((construct ((p1, q1), (p3, q3))), (p3, q3), (construct ((p3, q3), (p2, q2)))) in
    construct ((0, 1), (1, 0))

let value qt =
    match qt with
    | Qtree(qt1, r, qt2) -> r

let go_left qt =
    match qt with
    | Qtree(qt1, r, qt2) -> qt1

let go_right qt =
    match qt with
    | Qtree(qt1, r, qt2) -> qt2
