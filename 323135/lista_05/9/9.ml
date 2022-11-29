type empty = |

type _ fin_type =
| Empty : empty fin_type
| Unit : unit fin_type
| Bool : bool fin_type
| Pair : 'a fin_type * 'b fin_type -> ('a * 'b) fin_type
| Either : 'a fin_type * 'b fin_type -> ('a , 'b) Either.t fin_type


let rec all_values : type a. a fin_type -> a Seq.t = function 
    | Empty -> fun () -> Seq.Nil
    | Unit -> fun () -> Seq.Cons((), fun () -> Seq.Nil)
    | Bool -> fun () -> Seq.Cons(true, fun () -> Seq.Cons(false, fun () -> Seq.Nil))
    | Pair (a, b) -> let xs = all_values a 
        in Seq.concat (Seq.map (fun x -> Seq.map (fun v -> (v, x)) xs) (all_values b))
    | Either (a, b) -> let xs = all_values a
        in Seq.append (Seq.map (fun x -> Either.Left x) xs)
        (Seq.map (fun x -> Either.Right x) (all_values b))


let get_elements = List.of_seq;;
all_values (Pair(Unit, Bool)) |> get_elements;;
all_values (Pair(Bool, Bool)) |> get_elements;;
all_values (Pair(Bool, Pair(Unit, Bool))) |> get_elements;;
