type formula =
| Cond of formula * formula 
| Var of string 
| Spike


let rec string_of_formula f =
  match f with 
  | Spike -> "⊥"
  | Var s -> s 
  | Cond(phi, psi) -> 
    begin match phi with
    | Cond(_, _) -> "(" ^ (string_of_formula phi) ^ ") => " ^ (string_of_formula psi)
    | _ -> (string_of_formula phi) ^ " => " ^ (string_of_formula psi)
    end
;;

let pp_print_formula fmtr f =
  Format.pp_print_string fmtr (string_of_formula f)
;;

type theorem = formula list * formula 

let assumptions thm =
  fst thm 
;;

let consequence thm =
  snd thm 
;;

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

let by_assumption f =
  ([f], f)
;;

let imp_i f thm =
  (List.filter (fun x -> x <> f) (assumptions thm), Cond(f, consequence thm))
;;

let uniq_cons x xs = 
  if List.mem x xs then xs 
  else x :: xs 
;;

let imp_e th1 th2 =
  match consequence th1 with
  | Cond(phi, psi) -> (List.fold_right uniq_cons ((assumptions th1) @ (assumptions th2)) [], psi)
  | _ -> failwith "first argument must have a conditional as its consequence"
;; 

let bot_e f thm =
  match consequence thm with
  | Spike -> (assumptions thm, f)
  | _ -> failwith "theorem must have falsehood as its consequence"
;;

