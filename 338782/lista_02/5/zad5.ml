let rec merge ?(acc=[]) cmp xs ys = match (xs,ys) with
    | ([],[]) -> acc
    | ([],x::xs) | (x::xs,[]) -> merge cmp xs [] ~acc:(x::acc)
    | (x::xs as xss,(y::ys as yss)) -> if cmp x y then merge cmp xs yss ~acc:(x::acc) else merge cmp xss ys ~acc:(y::acc)

let rec halve = function
    | [] -> [],[]
    | [_] as xs -> xs,[[]]
    | (x::y::zs) -> let (xs,ys) = halve zs in x::xs,y::ys

let mergesort cmp xs = let rec megrepairs cmp = function
    | [] -> []
    | [x] -> x
    | xs -> let x,y = halve xs in
        List.map2 (merge cmp) x y |> megrepairs (Fun.flip cmp)  
    in let res = megrepairs cmp (List.map (fun x -> [x]) xs) in
        match res with
            | [] | [_] as xs -> xs
            | ((x::y::_) as xs) -> if cmp x y
                then xs
                else List.rev xs





