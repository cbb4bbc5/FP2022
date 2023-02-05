type formula = 
    | False
    | Variable of string
    | Implication of formula * formula


(* to jeszcze trzeba poprawic*)
let string_of_formula f =
    match f with
    | False -> "false"
    | Variable p -> p
    | Implication (f1, f2) -> 
           match f2 with
            | False -> (string_of_formula f1) ^ "-> false"
            | Variable q -> (string_of_formula f1) ^ "->" ^ q
            | Implication (a, b) -> "(" ^ (string_of_formula f1) ^ ") -> " ^ "(" ^ (string_of_formulaa) ^ ") -> " ^ (string_of_formula b)

let pp_print_formula fmtr f =
  Format.pp_print_string fmtr (string_of_formula f)

type theorem = {
    asumptions  : formula list;
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

(* ten filter jest chyba niepotrzebny i chyba nie dziala*)
let imp_i f thm =
  let ass = (assumptions thm) and con = (consequence thm) in
    { assumptions = List.filter (fun x -> if x == f then false else true) ass; consequence = Implication(f, con) }

let imp_e th1 th2 =
  let t = (consequence th1) and psi = (consequence th2) in
  match t with
  | Implication(a, b) -> 
          if b == psi then { assumptions = List.append (assumptions th1) (assumptions th2); consequence = a }
          else    failwith "lol"
  | False      -> failwith "lol"
  | Variable p -> failwith "lol"

let bot_e f thm =
  let ass = (assumptions thm) and con = (consequence thm) in
  match con with
    | False -> { consequence = ass; consequence = f }
    | Variable p -> failwith "lol"
    | Implication (a, b) -> failwith "lol"
