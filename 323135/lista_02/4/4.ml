(* podpunkt 1 *)
let rec merge cmp xs ys =
    match xs, ys with
    | [], _          -> ys
    | x::xs, []      -> x::xs
    | x::xs1, y::ys1 ->
        if cmp x y
            then x :: merge cmp xs1 ys
            else y :: merge cmp xs ys1;;

(* podpunkt 2 *)
let rec merge2 ?(acc = []) cmp xs ys =
    match xs, ys with
    | [], []         -> List.rev acc
    | [], y::ys      -> merge2 ~acc:(y::acc) cmp [] ys
    | x::xs, []      -> merge2 ~acc:(x::acc) cmp xs []
    | x::xs1, y::ys1 ->
        if cmp x y
            then merge2 ~acc:(x::acc) cmp xs1 ys
            else merge2 ~acc:(y::acc) cmp xs ys1

(* podpunkt 3 *)
let halve l = 
    let rec halve_final l1 l2 = 
        match l1, l2 with
            | h1::ls1, h21::h22::ls2 -> 
                let t = 
                    halve_final ls1 ls2 in [h1::(List.hd t); List.hd ((List.tl t))]
            | xs, _::[] | xs, []-> [[]; xs]
            | [], _ -> raise Not_found 
                in halve_final l l;;

(* podpunkt 4 *)
let rec mergesort cmp xs =
  match xs with
  | [] -> []
  | [x] -> [x]
  | _ ->
    let t = halve xs
    in merge cmp (mergesort cmp (List.hd t)) (mergesort cmp (List.hd (List.tl t)));;
