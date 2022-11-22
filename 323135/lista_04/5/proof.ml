open Logic

type goal = (string * formula) list * formula

(* zadanie 3 *)
(* typ opisujący potencjalnie niekompletne dowody bez celu *)
type ptree = 
  | Goal of goal
(* konstruktory dla reguł z listy 3 *)
  | ImpI of formula * ptree
  | ImpE of ptree * ptree
  | BotE of formula * ptree
(* dowody bez dziur *)
  | Final of theorem

(* typ kontekstów *)
type con = 
  | Root
  | Left of con * ptree
  | Right of ptree * con
  | ConImpI of formula * con
  | ConBotE of formula * con

type proof =
  | Complete of theorem
  | Incomplete of ptree * con

(* funkcja proof *)
let proof goal pf =
  Incomplete (Goal (goal, pf), Root)

(* funkcja goal *)
let goal pf = match pf
  | Complete _ -> None
  | Incomplete (Goal p, _) -> Some p
  | _ -> failwith "Not a goal"

(* funkcja qed *)
let qed = function 
  | Complete thm -> thm
  | Incomplete _ -> failwith "The proof is not complete"
(* koniec zadania 3 *)

(* zadanie 5 *)
let intro name pf = match pf with
  | Complete thm -> Complete thm
  | Incomplete(Goal (asm, Implication(p, q)), con) -> Incomplete(Goal ((name, p)::asm, q), ConImpI(p, con))
  | _ -> failwith "Goal is not implication"
(* koniec zadania 5 *)

(* formatter *)
let pp_print_proof fmtr pf =
  match goal pf with
  | None -> Format.pp_print_string fmtr "No more subgoals"
  | Some(g, f) ->
    Format.pp_open_vbox fmtr (-100);
    g |> List.iter (fun (name, f) ->
      Format.pp_print_cut fmtr ();
      Format.pp_open_hbox fmtr ();
      Format.pp_print_string fmtr name;
      Format.pp_print_string fmtr ":";
      Format.pp_print_space fmtr ();
      pp_print_formula fmtr f;
      Format.pp_close_box fmtr ());
    Format.pp_print_cut fmtr ();
    Format.pp_print_string fmtr (String.make 40 '=');
    Format.pp_print_cut fmtr ();
    pp_print_formula fmtr f;
    Format.pp_close_box fmtr ()
