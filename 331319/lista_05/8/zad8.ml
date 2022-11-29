type _ fin_type = 
| Unit : unit fin_type 
| Bool : bool fin_type 
| Pair : 'a fin_type * 'b fin_type -> ('a * 'b) fin_type 

let rec all_values : type a. a fin_type -> a Seq.t = 
  fun t -> 
  match t with 
  | Unit -> List.to_seq [()]
  | Bool -> List.to_seq [true; false]
  | Pair(t1, t2) -> Seq.product (all_values t1) (all_values t2)
;;
