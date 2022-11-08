type formula =
| Bot
| Var of string
| Imp of formula * formula

let rec string_of_formula f =
  match f with
  | Bot -> "⊥"
  | Var p -> p
  | Imp(l, r) ->
    let l_str, r_str = string_of_formula l, string_of_formula r
    in let l_str = match l with
        | Imp(_, _) -> String.concat "" ["("; l_str; ")"]
        | _ -> l_str
      in String.concat " → " [l_str; r_str]

let pp_print_formula fmtr f =
  Format.pp_print_string fmtr (string_of_formula f)

type theorem = Thm of formula list * formula

let assumptions thm =
  match thm with
  | Thm(assumptions, _) -> assumptions

let consequence thm =
  match thm with
  | Thm(_, consequence) -> consequence

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

let by_assumption f = Thm([f], f)

let imp_i f thm =
  match thm with
  | Thm(gamma, phi) ->
    Thm(List.filter (fun g -> not (g = f)) gamma, Imp(f, phi))

let imp_e th1 th2 =
  match th1, th2 with
  | Thm(gamma, Imp(phi, psi)), Thm(delta, tau) when phi = tau ->
    Thm(List.rev_append gamma delta, psi)
  | _, _ -> failwith "imp_e: Invalid proof"

let bot_e f thm =
  match thm with
  | Thm(gamma, Bot) -> Thm(gamma, f)
  | _ -> failwith "bot_e: Invalid proof"
