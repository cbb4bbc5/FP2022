let rec fold_left_cps f b xs cont =
    match xs with
    | [] -> cont b
    | h::tl -> f h b (fun v -> (fold_left_cps f v tl cont)) 

let fold_left f b xs = fold_left_cps (fun x xs cont -> f (cont xs) x) b xs (fun x -> x)
