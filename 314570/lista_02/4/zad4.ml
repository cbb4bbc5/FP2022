let rec merge cmp l1 l2 =
        begin
        match l1 with
        | [] -> begin
                match l2 with
                | [] -> []
                | (x::xs) -> x :: merge cmp l1 xs
                end
        | (x::xs) -> begin
                     match l2 with
                     | [] -> x :: merge cmp xs l2
                     | (y::ys) -> if (cmp x y)
                                  then x :: merge cmp xs l2
                                  else y :: merge cmp l1 ys
                     end
        end ;;

let merge_tail cmp l1 l2 = 
        let rec merge_tail_acc cmp l1 l2 acc =
        begin
        match l1 with
        | [] -> begin
                match l2 with
                | [] -> acc
                | (y::ys) -> merge_tail_acc cmp l1 ys (y::acc)
                end
        | (x::xs) -> begin
                     match l2 with
                     | [] -> merge_tail_acc cmp xs l2 (x::acc)
                     | (y::ys) -> if (cmp x y)
                                  then merge_tail_acc cmp xs l2 (x::acc)
                                  else merge_tail_acc cmp l1 ys (y::acc)
                     end
        end
        in List.rev (merge_tail_acc cmp l1 l2 []) ;;

let halve lst = 
        let rec halve_rec lst acc = 
                if abs ((List.length lst) - (List.length acc)) <= 1
                then (List.rev acc, lst)
                else halve_rec (List.tl lst) ((List.hd lst) :: acc)
        in halve_rec lst [] ;;



let rec mergesort lst = 
        match lst with
        | [] -> []
        | [x] -> [x]
        | xs -> let (left, rigth) = (halve xs) in merge_tail (<=) (mergesort left) (mergesort right) ;;

