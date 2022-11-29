type _ fin_type =
| Unit : unit fin_type
| Bool : bool fin_type
| Pair : 'a fin_type * 'b fin_type -> ('a * 'b) fin_type

let rec all_values : type a. a fin_type -> a Seq.t =
  function
  | Unit -> Seq.cons () Seq.empty
  | Bool -> Seq.empty |> Seq.cons false |> Seq.cons true
  | Pair (ft1, ft2) ->
    Seq.flat_map (fun y -> Seq.map (fun x -> (x, y)) (all_values ft1)) (all_values ft2)

let rec to_string : type a. a fin_type -> a -> string =
  fun ft v -> match ft with
  | Unit -> "()"
  | Bool -> string_of_bool v
  | Pair (ft1, ft2) -> "(" ^ (to_string ft1 (fst v)) ^ ", " ^ (to_string ft2 (snd v)) ^ ")"

let rec print_ft_val ft v = Format.printf "%s\n" (to_string ft v)

let ft = (Pair((Pair(Bool, (Pair(Bool, Unit)))), (Pair((Pair(Unit, Bool)), Bool)))) ;;
let s  = all_values ft ;;
Seq.iter (print_ft_val ft) s ;;
