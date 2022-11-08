type formula = 
  | Bottom 
  | Var of string
  | Impl of formula * formula

let string_of_formula f =
  (* TODO: zaimplementuj *)
  let rec string_of_formula_rec f =
    match f with
    | Bottom -> "⊥"
    | Var name -> name 
    | Impl(left, right) -> 
        ((match left with
         | Impl (_,_) -> "("^ (string_of_formula_rec left) ^")"
         | _ -> string_of_formula_rec left)
        ^ " → "^ string_of_formula_rec right) 
    in string_of_formula_rec f

let pp_print_formula fmtr f =
  Format.pp_print_string fmtr (string_of_formula f)

type theorem = Thm of formula list * formula

let assumptions thm = match thm with Thm(assumptions, _) -> assumptions 
let consequence thm = match thm with Thm(_,consequence) -> consequence 

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

let imp_i f thm = Thm(List.filter (fun x -> x <> f) (assumptions thm), Impl(f,consequence thm))

let imp_e th1 th2 = 
  match consequence th1, consequence th2 with
  | Impl(fi1,psi), fi2 when fi1 = fi2 -> 
      Thm(List.fold_left 
          (fun acc x -> if List.mem x acc 
                                       then acc 
                                       else x::acc) 
          (assumptions th1)
          (assumptions th2),
      psi)
  | _ -> failwith "Theorems don't match specification"

let bot_e f thm = 
  match thm with
    | Thm(assumptions,Bottom) -> Thm(assumptions, f)
    | _ -> failwith "Theorem consequence isn't a Bottom" 
