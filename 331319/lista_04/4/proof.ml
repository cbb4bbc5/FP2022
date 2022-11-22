open Logic

type ass = (string * formula) list

type in_progress = 
| Goal of ass * formula 
| Ready of theorem 
| CondI of in_progress * ass * formula 
| CondE of in_progress * in_progress * ass 
| SpikeE of in_progress * ass * formula 

type context = 
| Root
| DownCond of context * ass * formula
| DownSpike of context * ass * formula 
| Left of context * ass * in_progress 
| Right of in_progress * ass * context 

type proof = 
| Complete of in_progress 
| WithGoal of in_progress * context 

let proof g f =
  WithGoal(Goal(g, f), Root)
;;

let rec qed pf =
  match pf with 
  | Complete(ppf) -> 
    begin match ppf with 
    | Goal(_, _) -> failwith "qed: ??"
    | Ready(thm) -> thm 
    | CondI(ppf1, _, phi) -> imp_i phi (qed (Complete ppf1))
    | CondE(ppf1, ppf2, _) -> imp_e (qed (Complete ppf1)) (qed (Complete ppf2))
    | SpikeE(ppf1, _, phi) -> bot_e phi (qed (Complete ppf1))
    end
  | WithGoal(_, _) -> failwith "Proof incomplete!"
;;

let rec goal_aux ppf = 
  match ppf with 
  | Goal(gamma, phi) -> Some (gamma, phi)
  | Ready _ -> None 
  | CondI(ppf1, _, _) | SpikeE(ppf1, _, _) -> goal_aux ppf1 
  | CondE(ppf1, ppf2, _) ->
    let res = goal_aux ppf1 in 
    begin match res with 
    | Some(_, _) -> res
    | None -> goal_aux ppf2 
    end
;;

let rec goal pf =
  match pf with 
  | WithGoal(ppf, ctx) -> goal_aux ppf 
  | Complete _ -> None 
;;

let rec seek_down ppf ctx = 
  match ppf with 
  | Goal(_, _) -> Some(WithGoal(ppf, ctx))
  | Ready _ -> None 
  | CondI(ppf1, gamma, phi) -> seek_down ppf1 (DownCond(ctx, gamma, phi))
  | CondE(ppf1, ppf2, gamma) -> 
    let res = seek_down ppf1 (Left(ctx, gamma, ppf2)) in
    begin match res with 
    | Some _ -> res 
    | None -> seek_down ppf2 (Right(ppf1, gamma, ctx)) 
    end
  | SpikeE(ppf1, gamma, phi) -> seek_down ppf1 (DownSpike(ctx, gamma, phi))
;;

let rqc ppf = Ready(qed (Complete ppf))

let rec seek_up ppf ctx = 
  match ctx with 
  | Root -> begin match seek_down ppf Root with Some pf -> pf | None -> Complete ppf end 
  | DownCond(ctx, gamma, phi) -> seek_up  (rqc (CondI(ppf, gamma, phi))) ctx 
  | Left(ctx, gamma, ppf1) -> 
    begin match seek_down ppf1 (Right(ppf, gamma, ctx)) with 
    | Some(pf) -> pf 
    | None -> seek_up (rqc (CondE(ppf, ppf1, gamma))) ctx (* (CondE(ppf, ppf1, gamma)) *)
    end
  | Right(ppf1, gamma, ctx) -> seek_up (rqc (CondE(ppf1, ppf, gamma))) ctx 
  | DownSpike(ctx, gamma, phi) -> seek_up (rqc (SpikeE(ppf, gamma, phi))) ctx 
;;

(* Jeśli siedzimy na aktywnym celu, szukamy innego. *)
let next pf =
  match pf with 
  | WithGoal(ppf, ctx) -> 
    begin match ppf with 
    | Goal(_, _) -> seek_up ppf ctx 
    | _ -> 
      begin match seek_down ppf ctx with 
      | Some(pf) -> pf 
      | None -> seek_up (rqc ppf) ctx (* Dodałem qed *)
      end
    end
  | Complete(ppf) -> failwith "No more goals"
;;

let intro name pf =
  match pf with 
  | WithGoal(ppf, ctx) ->
    begin match ppf with 
    | Goal(gamma, phi) -> 
      begin match phi with 
      | Cond(ant, csq) -> next (WithGoal(CondI(Goal((name, ant) :: gamma, csq), gamma, ant), ctx)) (* next? *)
      | _ -> failwith "Not a conditional"
      end
    | _ -> failwith "intro: ??"
    end
  | Complete(ppf) -> failwith "Nothing to introduce"
;;

(* Zgaduję, że to działa *)
let apply_form fm s pf =
  match pf with 
  | WithGoal(Goal(gamma, phi), ctx) ->
    let rec go f acc = 
      match f with 
      | Cond(psi, f') when f <> phi -> go f' (CondE(acc, Goal(gamma, psi), gamma))
      | Spike -> SpikeE(acc, gamma, phi)
      | _ -> acc 
    in next (WithGoal(go (fm gamma) (s gamma phi), ctx)) (* next? ale co, gdy n = 0? *)
  | _ -> failwith "apply: complete"
;;

let apply f pf = apply_form (fun gamma -> f) 
                            (fun gamma phi -> Goal(gamma, phi)) 
                            pf 
;;

let apply_thm thm pf = apply_form (fun gamma -> (consequence thm)) 
                       (fun gamma phi -> Ready thm) 
                       pf 
;;

let apply_assm name pf = apply_form (fun gamma -> consequence (by_assumption (List.assoc name gamma))) 
                         (fun gamma phi -> Ready(by_assumption (List.assoc name gamma)))
                         pf
;;

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
