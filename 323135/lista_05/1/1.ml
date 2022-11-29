let rec fix f x = f (fix f) x

let rec fix_with_limit n f x = 
    if n = 0
        then failwith "Maximum recursion depth exceeded"
    else f (fix_with_limit (n - 1) f) x

let fix_memo f =
    let tbl = Hashtbl.create 1 in
    let rec fix_memo_aux x = 
        match Hashtbl.find_opt tbl x with
        | None -> let y = f fix_memo_aux x in Hashtbl.add tbl x y; y
        | Some y -> y
    in fix_memo_aux
