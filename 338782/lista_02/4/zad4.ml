let rec merge cmp xs ys = match (xs,ys) with
    | ([],xs) | (xs,[]) -> xs
    | (x::xs as xss,(y::ys as yss)) -> if cmp x y then x :: merge cmp xs yss else y :: merge cmp xss ys

let rec merge_tail ?(acc=[]) cmp xs ys = match (xs,ys) with
    | ([],xs) | (xs,[]) -> List.rev_append xs acc 
    | (x::xs as xss,(y::ys as yss)) -> if cmp x y then merge_tail cmp xs yss ~acc:(x::acc) else merge_tail cmp xss ys ~acc:(y::acc)

let rec halve = function
    | [] -> [],[]
    | [_] as xs -> xs,[]
    | (x::y::zs) -> let (xs,ys) = halve zs in x::xs,y::ys

let rec mergesort merge cmp = function
    | [] -> []
    | [x] -> [x]
    | xs -> let (x,y) = halve xs in merge cmp (mergesort merge cmp x) (mergesort merge cmp y)

