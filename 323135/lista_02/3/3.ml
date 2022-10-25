let rec suffix l =
    let rec suf lt acc = 
        match lt with
            | [] -> acc
            | x::xs -> suf xs (List.append acc [xs]) in suf l [l];;

let rec prefixes xs =
    match xs with
    | []    -> [[]]
    | l::ls -> [] :: List.map (fun ts -> l::ts) (prefixes ls);;
