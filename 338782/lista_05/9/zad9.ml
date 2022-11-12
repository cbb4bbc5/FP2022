type empty = |

type _ fin_type =
| Unit : unit fin_type
| Bool : bool fin_type
| Pair : 'a fin_type * 'b fin_type -> ('a * 'b) fin_type
| Either : 'a fin_type * 'b fin_type -> ('a , 'b) Either.t fin_type
| Empty : empty fin_type

let rec all_values : type c. c fin_type -> c Seq.t = function 
    | Unit -> fun () -> Seq.Cons((),fun () -> Seq.Nil)
    | Bool -> fun () -> Seq.Cons(true,fun () -> Seq.Cons(false,fun () -> Seq.Nil))
    | Pair(a,b) -> let ys = all_values a in Seq.flat_map (fun x -> Seq.map (fun elt -> (elt,x)) ys) (all_values b)
    | Either(a,b) -> Seq.append (Seq.map (fun x -> Either.Left x) (all_values a)) (Seq.map (fun x -> Either.Right x) (all_values b))
    | Empty -> fun () -> Nil

