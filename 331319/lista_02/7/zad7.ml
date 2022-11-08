type 'a tree = 
| Leaf
| Node of 'a tree * 'a * int * 'a tree 

let smart_cons t1 t2 elt = 
  match t1, t2 with
  | Leaf, Leaf -> Node(Leaf, elt, 1, Leaf)
  | Leaf, (Node(l, e, s, r) as t) | (Node(l, e, s, r) as t), Leaf -> Node(t, elt, 1, Leaf)
  | Node(l1, e1, s1, r1), Node(l2, e2, s2, r2) ->
    if s1 < s2 then Node(t2, elt, s1 + 1, t1)
    else Node(t1, elt, s2 + 1, t2)
;;

let rec merge t1 t2 = 
  match t1, t2 with
  | Leaf, t | t, Leaf -> t
  | Node(l1, e1, s1, r1), Node(l2, e2, s2, r2) ->
    if e1 < e2 then smart_cons l1 (merge r1 t2) e1
    else smart_cons l2 (merge r2 t1) e2
;;

let insert elt t = merge t (Node(Leaf, elt, 1, Leaf))

let delete_min t = 
  match t with
  | Leaf -> t 
  | Node(l, e, s, r) -> merge l r
;;
