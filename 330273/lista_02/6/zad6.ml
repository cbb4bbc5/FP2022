let rec permutations ls =
    let n = List.length ls in
    if n = 1 then [ls] else
        let perms = permutations (List.tl ls) and x = List.hd ls in
        let rec insert_everywhere x ls =
             match ls with
                | [] -> [[x]]
                | h::t -> (x :: ls) :: (List.map (fun a -> h :: a) (insert_everywhere x t)) 
        in
        let rec iter l =
            match l with
            | [] -> []
            | h::t -> (insert_everywhere x h) @ (iter t)
        in iter perms
                

