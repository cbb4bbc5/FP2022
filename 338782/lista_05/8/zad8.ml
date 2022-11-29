type _ fin_type =
| Unit : unit fin_type
| Bool : bool fin_type
| Pair : 'a fin_type * 'b fin_type -> ('a * 'b) fin_type


let rec all_values : type c. c fin_type -> c Seq.t = function 
    | Pair(a,b) -> let ys = all_values a in Seq.flat_map (fun x -> Seq.map (fun elt -> (elt,x)) ys) (all_values b)
    | Unit -> fun () -> Seq.Cons((),fun () -> Seq.Nil)
    | Bool -> fun () -> Seq.Cons(true,fun () -> Seq.Cons(false,fun () -> Seq.Nil))

