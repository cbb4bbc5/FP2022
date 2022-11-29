let rec fix f x = f (fix f) x

let fib_f fib n = 
    if n<= 1 then n
    else fib(n-1) + fib(n-2)



let rec fix_with_limit n f x = 
    if n<0 then failwith "Limit przekroczony"
    else f (fix_with_limit (n-1) f) x


let fix_memo f =
    let tab = Hashtbl.create 10 in
    let rec memo x = 
        match Hashtbl.find_opt tab x with
        | None -> let y = f memo x in Hashtbl.add tab x y;y
        | Some y -> y
    in memo
