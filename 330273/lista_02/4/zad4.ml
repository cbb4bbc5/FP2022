let halve l =
  let rec aux l left right = 
    match l,left,right with
    | ([] | [_]),_,_ -> (List.rev left),right
    | (_::_::t),_,h::right_t -> aux t (h::left) right_t
    | _ -> assert false
  in
  aux l [] l

let rec merge_tail comp l1 l2 = 
    let rec iter l l1 l2 =
        match l1,l2 with
        | [],t | t,[] -> (List.rev t) @ l
        | h1::t1, h2::t2 -> 
            if comp h1 h2 then (iter (h1::l) t1 l2)
            else (iter (h2::l) l1 t2)
    in List.rev (iter [] l1 l2)

let rec merge_tail2 comp l1 l2 = 
    let new_comp = fun x y -> not (comp x y) in
    let rec iter l l1 l2 =
        match l1,l2 with
        | [],t | t,[] -> l @ t
        | h1::t1, h2::t2 -> 
            if new_comp h1 h2 then (iter (h1::l) t1 l2)
            else (iter (h2::l) l1 t2)
    in iter [] l1 l2

let rec merge comp l1 l2 =
  match l1,l2 with
  | [],l | l,[] -> l
  | h1::t1, h2::t2 ->
        if comp h1 h2 then h1::(merge comp t1 l2)
        else h2::(merge comp l1 t2)

let rec mergesort comp ls =
    let halves = halve ls in
    let l1 = fst halves and l2 = snd halves in
    match l1,l2 with
    | [],l | l,[] -> l
    | h1::t1, h2::t2 -> merge comp (mergesort comp l1) (mergesort comp l2)
