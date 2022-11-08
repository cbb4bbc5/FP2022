(* ZADANIE III *)
type formula = 
  | False
  | Var of string
  | Imply of formula * formula

(* ZADANIE IV *)
let rec string_of_formula = function (* TODO: zaimplementuj *)
  | False -> "⊥"
  | Var(v) -> v
  | Imply(p, q) -> let pstr = string_of_formula p in
    begin match p with
    | Imply(_, _) -> "(" ^ pstr ^ ")"
    | _ -> pstr end ^ " -> " ^ string_of_formula q;;

let pp_print_formula fmtr f =
  Format.pp_print_string fmtr (string_of_formula f)

(* ZADANIE V *)
type theorem = Theorem of formula list * formula

let assumptions = function Theorem(fs, p) -> fs

let consequence = function Theorem(fs, p) -> p

let pp_print_theorem fmtr thm =
  let open Format in
  pp_open_hvbox fmtr 2;
  begin match assumptions thm with
  | [] -> ()
  | f :: fs ->
    pp_print_formula fmtr f;
    fs |> List.iter (fun f ->
      pp_print_string fmtr ",";
      pp_print_space fmtr ();
      pp_print_formula fmtr f);
    pp_print_space fmtr ()
  end;
  pp_open_hbox fmtr ();
  pp_print_string fmtr "⊢";
  pp_print_space fmtr ();
  pp_print_formula fmtr (consequence thm);
  pp_close_box fmtr ();
  pp_close_box fmtr ()

(* ZADANIE VI *)
let rec same f q = match f, q with
  | Imply(p, q), Imply(r, s) -> same p r || same q s
  |           Var(v), Var(w) -> v = w
  |             False, False -> true
  |                     _, _ -> false

let not_same f q = not (same f q)

let by_assumption f = Theorem([f], f)

let imp_i f = function 
  | Theorem(fs, p) -> Theorem(List.filter (not_same f) fs, Imply(f, p))

let imp_e = function
  | Theorem(fs, Imply(p, q)) -> function
    | Theorem(qs, r) when same p r -> Theorem(fs @ qs, q)

let bot_e f = function 
  | Theorem(fs, False) -> Theorem(fs, f)

(* ZADANIE VII *)
let p = Var "p" and q = Var "q" and r = Var "r" and n = False;;
let pp = Imply (p, p);;
let pqp = Imply (p, Imply (q, p));;
let pqr = Imply (p, Imply (q, r));;
let pq = Imply (p, q);;
let pqrpqpr = Imply (pqr, Imply (pq, Imply (p, r)));;
let np = Imply (n, p);;
let z71 = Theorem ([], pp)
and z72 = Theorem ([], pqp)
and z73 = Theorem ([], pqrpqpr)
and z74 = Theorem ([], np);;