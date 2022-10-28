type 'a clist = { clist : 'z. ('a -> 'z -> 'z) -> 'z -> 'z }
let cnil = {clist = fun f z -> z}
let ccons x xs = {clist = fun f z -> f x (xs.clist f z)}
let map f0 xs = {clist = fun f z -> xs.clist (fun a b -> f (f0 a) b) z}
let append a b = {clist = fun f z -> a.clist f (b.clist f z)}
let clist_to_list c = c.clist List.cons []
let list_to_clist l = {clist = fun f z -> List.fold_right f l z}
let prod a b = {clist = fun f z -> a.clist (fun xa ya -> b.clist (fun xb yb -> f (xa,xb) yb) ya) z}
let rec pow a = function 
    | 0 -> {clist = fun f z -> f [] z}
    | n -> let prev = pow a (n-1) in
        {clist = fun f z -> a.clist (fun xa ya -> prev.clist (fun x1 y1 -> f (xa::x1) y1) ya) z}

