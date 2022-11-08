let rec sufixes lst = 
        match lst with
        | [] -> [[]]
        | (_::xs) -> lst :: (sufixes xs) ;;

let prefixes lst = 
    let rec prefixes_acc lst acc = 
            match lst with 
            | [] -> [acc]
            | (x::xs) -> acc :: (prefixes_acc xs (acc @ [x]))
    in prefixes_acc lst [] ;;
