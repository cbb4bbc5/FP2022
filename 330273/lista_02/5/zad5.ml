let halve l =
  let rec aux l left right = 
    match l,left,right with
    | ([] | [_]),_,_ -> (List.rev left),right
    | (_::_::t),_,h::right_t -> aux t (h::left) right_t
    | _ -> assert false
  in
  aux l [] l

let rec merge comp b l1 l2 = 
    let rec iter l l1 l2 =
        match l1,l2 with
        | [],[] -> l
        | h::t,[] | [],h::t -> iter (h::l) t [] 
        | h1::t1, h2::t2 -> 
            if b <> (comp h1 h2) then (iter (h1::l) t1 l2)
            else (iter (h2::l) l1 t2)
    in iter [] l1 l2
 
let mergesort comp ls =
    let rec aux ls b =
         let halves = halve ls in
         let l1 = fst halves and l2 = snd halves in
            match l1,l2 with
            | [],l | l,[] -> l
            | h1::t1, h2::t2 -> merge comp b (aux l1 (not b)) (aux l2 (not b))  
    in aux ls true

let list1 = 
    let rec aux k =
        if (k < 100000) then (Random.int 1000000) :: (aux (k + 1)) else []
    in
    aux 0
