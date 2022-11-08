(* zadanie 3 *)
type formula =
  | Bot
  | Variable of string
  | Implication of formula * formula
(* koniec zadania 3 *)

(* zadanie 4 *)
let rec string_of_formula f = match f with
  | Bot -> "⊥"
  | Variable x -> x
  | Implication ((Implication _) as a,b) -> "(" ^
                                            string_of_formula a ^
                                            ") → " ^
                                            string_of_formula b
  | Implication (p, q) -> string_of_formula p ^
                          " → " ^
                          string_of_formula q

let pp_print_formula fmtr f =
  Format.pp_print_string fmtr (string_of_formula f)

(* zadanie 5 *)
module Form =
  struct
    type t = formula
    let compare a b = match (a,b) with
      | (Bot, Bot) -> 0
      | (Bot, _) -> -1
      | (_, Bot) -> 1
      | (Variable a, Variable b) -> Stdlib.compare a b
      | (Variable _, _) -> -1
      | (_, Variable _) -> 1
      | (Implication (p1, p2), Implication (b1, b2)) ->
          match Stdlib.compare p1 b1 with
            | 0 -> Stdlib.compare p2 b2
            | x -> x
  end
module Asm = Set.Make(Form)
type theorem = Asm.t * formula

let assumptions thm = Asm.elements(fst thm)

let consequence thm = snd thm

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
(* koniec zadania 5 *)

(* zadanie 6 *)
let by_assumption f = Asm.add f Asm.empty, f

let imp_i f thm = Asm.remove f (fst thm), Implication(f, consequence thm)

let imp_e th1 th2 =
  match consequence th1 with
  | Bot | Variable _ -> failwith "invalid action"
  | Implication(p, q) ->
     if Form.compare p (consequence th2) = 0
     then Asm.union (fst th1) (fst th2), q
     else failwith "false"

let bot_e f thm =
  match consequence thm with
  | Bot -> fst thm, f
  | _ -> failwith "false"
(* koniec zadania 6 *)

(* zadanie 7 *)
let p = Variable "p"
let q = Variable "q"
let r = Variable "r"

(* podpunkt 1 *)
let f1 = Implication(p, p)
let proof1 = by_assumption p |> imp_i p

(* podpunkt 2 *)
let f2 = Implication(p, Implication(q, p))
let proof2 = by_assumption p |> imp_i q |> imp_i p

(* podpunkt 3 *)
let f31 = Implication(p, Implication(q, r))
let f321 = Implication(p, q)
let f322 = Implication(p, r)
let f32 = Implication(f321, f322)
let f3 = Implication(f31, f32)

let proof311 = imp_e (by_assumption f31) (by_assumption p)
let proof312 = imp_e (by_assumption f321) (by_assumption p)
let proof31 = imp_e proof311 proof312
let proof3 = proof31
             |> imp_i p 
             |> imp_i f321
             |> imp_i f31

(* podpunkt 4 *)
let f4 = Implication(Bot, p)
let proof4 = by_assumption Bot |> bot_e p |> imp_i Bot
