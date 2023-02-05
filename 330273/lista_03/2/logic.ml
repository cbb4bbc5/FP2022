type formula = 
    | False
    | Variable of string
    | Implication of formula * formula

let rec string_of_formula f =
    match f with
    | False -> "false"
    | Variable p -> p
    | Implication (f1, f2) -> 
           match f1 with
            | False -> (string_of_formula f1) ^ "-> " ^ (string_of_formula f2)
            | Variable q -> q ^ "-> " ^ (string_of_formula f2)
            | Implication (a, b) -> "(" ^ (string_of_formula f1) ^ ") -> " ^ (string_of_formula f2)

let pp_print_formula fmtr f =
  Format.pp_print_string fmtr (string_of_formula f)

type theorem = {
    assumptions : formula list;
    consequence : formula
}

let assumptions thm =
    thm.assumptions

let consequence thm =
    thm.consequence

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
    { assumptions = [f]; consequence = f }

let imp_i f thm =
    let ass = (assumptions thm) and con = (consequence thm) in
    { assumptions = List.filter (fun x -> if x = f then false else true) ass; consequence = Implication(f, con)  }

let imp_e th1 th2 =
    let ass1 = (assumptions th1) and con1 = (consequence th1) in
    let ass2 = (assumptions th2) and con2 = (consequence th2) in
    match con1 with
        | False              -> failwith "error"
        | Variable _         -> failwith "error"
        | Implication (a, b) -> 
                if con2 = a then
                { assumptions = List.append ass1 ass2; consequence = b }
                else failwith "error"

let bot_e f thm =
    let ass = (assumptions thm) and con = (consequence thm) in
    match con with
        | False            -> { assumptions = ass; consequence = f }
        | Variable _       -> failwith "error"
        | Implication(_,_) -> failwith "error" 
