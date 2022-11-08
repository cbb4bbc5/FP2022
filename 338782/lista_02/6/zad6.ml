let rec permIns i = function [] -> [[i]] | (x::xs) as lxs -> let rest = permIns i xs in (i::lxs)::(List.map (List.cons x) rest)
let rec permsIns = function [] -> [[]] | x::xs -> List.concat (List.map (permIns x) (permsIns xs ))
let rec permSelect = function [] ->[] | x::xs as lxs -> (x,xs)::List.map (fun (b,bs) -> b,x::bs) (permSelect lxs)
let rec permsSelect = function [] -> [[]] | x::xs -> List.concat (List.map (fun (b,bs) -> List.map (List.cons b) (permsSelect bs)) (permSelect xs))

