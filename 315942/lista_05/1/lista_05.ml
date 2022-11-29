(* Zadanie I *)
let rec fix f x = f (fix f) x

let fib_f fib n =
    if n <= 1 then n
    else fib (n-1) + fib (n-2)

exception DepthLimit of string

let rec fix_with_limit limit f x = 
    if (limit > 0)
    then f (fix_with_limit (limit-1) f) x
    else raise (DepthLimit "Maximum recursion depth reached")

let fix_memo f x =
    let mem = Hashtbl.create(10)
    in let rec fix_with_mem f x =
           try Hashtbl.find mem x with
           | Not_found -> let fx = f (fix_with_mem f) x
                          in begin Hashtbl.add mem x fx; fx end
       in fix_with_mem f x

(* Zadanie III *)

type 'a lazyNode = Node of 'a * ('a -> 'a lazyNode)

let get_val = function Node ((a, b, c, d), f) -> (a+c), (b+d);;
let left_child = function Node ((a, b, c, d), f) -> f (a, b, a+c, b+d);;
let right_child = function Node ((a, b, c, d), f) -> f (a+c, b+d, c, d);;
let rec node x = Node (x, node);;
let aprnt = Node ((0, 1, 1, 0), node);;


(* Zadanie VIII + IX*)
type empty = |;;

type _ fin_type =
| Empty : empty fin_type
| Unit : unit fin_type
| Bool : bool fin_type
| Pair : 'a fin_type * 'b fin_type -> ('a * 'b) fin_type
| Either: 'a fin_type * 'b fin_type ->  ('a, 'b) Either.t fin_type

let rec all_values : type a. a fin_type -> a Seq.t = function
| Empty -> Seq.empty
| Unit -> Seq.return ()
| Bool -> Seq.cons true (Seq.return false)
| Pair (seq1, seq2) -> Seq.fold_left (fun a b -> 
    Seq.append (Seq.fold_left (fun x y -> 
        Seq.cons (b, y) x
    ) Seq.empty (all_values seq2)) a
) Seq.empty (all_values seq1)
| Either (seq1, seq2) -> Seq.fold_left (fun a b -> 
    Seq.cons (Either.left b) a
) (Seq.fold_left (fun a b ->
    Seq.cons (Either.right b) a
) Seq.empty (all_values seq2)) (all_values seq1)

let fold = Seq.fold_left (fun a b -> b :: a) [];;