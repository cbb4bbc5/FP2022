type 'a leftist_tree =
    | Leaf
    | Node of 'a leftist_tree * 'a * 'a leftist_tree * int

let singleton a = Node (Leaf, a, Leaf, 1)
let rank t = 
    match t with
    | Leaf -> 0
    | Node (_,_,_,r) -> r

let rec merge t1 t2 =
    match t1,t2 with
    | Leaf, t | t, Leaf -> t
    | Node (l, a1, r, _), Node (_, a2, _, _) ->
            if a1 > a2 then merge t2 t1
            else
                let merged = merge r t2 in
                let rank_l = rank l and rank_r = rank merged in
                if rank_l < rank_r then Node (merged, a1, l, rank_l + 1)
                else Node (l, a1, merged, rank_r + 1)

let insert x t = merge (singleton x) t
let get_min t = 
    match t with
    | Leaf -> failwith "empty"
    | Node (_, a, _, _) -> a

let delete_min t =
    match t with
    | Leaf -> failwith "empty"
    | Node (l, _, r, _) -> merge l r

module PQ : sig    
let push x pq =
    merge (singleton x) pq

let top pq = get_min pq

let pop pq = (get_min pq), (delete_min pq)
