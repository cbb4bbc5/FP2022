type 'a ltree = unit -> 'a lnode
and  'a lnode = Leaf 
              | Node of 'a ltree * 'a * 'a ltree 

type frac = int * int 

let left lnd = match lnd with Leaf -> failwith "left: leaf" | Node(llt, _, _) -> llt 
let right lnd = match lnd with Leaf -> failwith "right: leaf" | Node(_, _, rlt) -> rlt 

let rec fractions a b = fun () -> 
  let q = (fst a + fst b, snd a + snd b) in
  Node (fractions a q, q, fractions q b)
;;

let rationals = fractions (0,1) (1,0) ()
