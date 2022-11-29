type empty = |

type _ fin_type =
| Empty : empty fin_type
| Unit : unit fin_type
| Bool : bool fin_type
| Pair : 'a fin_type * 'b fin_type -> ('a * 'b) fin_type
| Either : 'a fin_type * 'b fin_type -> ('a, 'b) Either.t fin_type

let rec all_values : type a. a fin_type -> a Seq.t = function
  | Empty -> Seq.empty
  | Unit -> Seq.empty |> Seq.cons ()
  | Bool -> Seq.empty |> Seq.cons true |> Seq.cons false
  | Either(t1,t2) -> Seq.append (Seq.map (fun elem -> Either.left elem) (all_values t1)) (Seq.map (fun elem -> Either.right elem) (all_values t2))
  | Pair(t1,t2) -> Seq.fold_left 
                   (fun acc_l elem_l -> 
                     Seq.fold_left 
                     (fun acc_r elem_r -> Seq.cons (elem_l, elem_r) acc_r) 
                     acc_l (all_values t2)) 
                   Seq.empty (all_values t1) 
