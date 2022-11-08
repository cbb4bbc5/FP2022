let rec sublists lst = 
        match lst with
        | [] -> [[]]
        | (x::xs) -> let lst = (sublists xs) in List.map (fun elem -> x::elem) lst @ lst ;;
