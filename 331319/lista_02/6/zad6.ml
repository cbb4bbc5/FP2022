
(* Algorytm Steinhausa-Johnsona-Trottera -- wstawianie *)
(* Wstawia element na pozycję i *)
let insert xs elt i = 
  let rec go xs k = 
    match xs with 
    | [] -> [elt]
    | hd :: tl when k = i -> elt :: xs
    | hd :: tl -> hd :: (go tl (k + 1))
  in go xs 0
;;

(* Generuje listę wstawień elementu *)
let insertions xs elt = 
  List.init ((List.length xs) + 1) (fun x -> insert xs elt x)
;;

(* Dorzuca element do listy xs permutacji na wszystkie sposoby *)
let rec perm_step xs elt = 
  match xs with 
  | [] -> [] 
  | x :: xs -> List.append (insertions x elt) (perm_step xs elt)
;;

let rec perms_sjt xs = 
  match xs with
  | [] -> [[]]
  | x :: xs -> perm_step (perms_sjt xs) x
;;
