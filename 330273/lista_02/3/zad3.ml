let rec suffixes xs = 
    match xs with
    | [] -> [[]]
    | x :: tail -> [x :: tail] @ ( suffixes tail )
 
let prefixes xs = List.map List.rev (suffixes (List.rev xs))
