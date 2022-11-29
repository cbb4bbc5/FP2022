type empty = |

type _ fin_type =
| Empty  : empty fin_type
| Unit   : unit  fin_type
| Bool   : bool  fin_type
| Pair   : 'a fin_type * 'b fin_type -> ('a * 'b) fin_type
| Either : 'a fin_type * 'b fin_type -> ('a,  'b) Either.t fin_type

let rec all_values : type a. a fin_type -> a Seq.t =
  function
  | Empty  -> Seq.empty
  | Unit   -> Seq.return ()
  | Bool   -> Seq.return false |> Seq.cons true
  | Pair (ft1, ft2) ->
    Seq.flat_map (fun y -> Seq.map (fun x -> (x, y)) (all_values ft1)) (all_values ft2)
  | Either (ft1, ft2) ->
    let s1 = Seq.map (fun x -> Either.Left  x) (all_values ft1) in
    let s2 = Seq.map (fun y -> Either.Right y) (all_values ft2) in
    Seq.concat (Seq.cons s1 (Seq.return s2))

let rec to_string : type a. a fin_type -> a -> string =
  fun ft v -> match ft with
  | Empty  -> "."
  | Unit   -> "()"
  | Bool   -> string_of_bool v
  | Pair (ft1, ft2)   -> "(" ^ (to_string ft1 (fst v)) ^ ", " ^ (to_string ft2 (snd v)) ^ ")"
  | Either (ft1, ft2) -> begin
      match v with
      | Either.Left  v -> to_string ft1 v
      | Either.Right v -> to_string ft2 v
    end

let rec print_ft_val ft v = Format.printf "%s\n" (to_string ft v)

let ft = (Pair((Either(Bool, Unit)), (Either((Pair(Bool, Bool)), (Either(Bool, Empty)))))) ;;
let s = all_values ft ;;
Seq.iter (print_ft_val ft) s ;;
