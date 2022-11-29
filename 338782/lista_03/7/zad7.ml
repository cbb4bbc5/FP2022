type formula =
  | Var of string
  | Bot 
  | Imp of formula * formula

let rec string_of_formula = function
  | Bot -> "⊥"
  | Var p -> p
  | Imp ((Imp _) as a,b) -> "("^string_of_formula a^") -> "^string_of_formula b
  | Imp (a,b) -> string_of_formula a^" -> "^string_of_formula b

module Formulas =
  struct
    type t = formula
    let compare a b = match (a,b) with
      | (Bot,Bot) -> 0
      | (Bot,_) -> -1
      | (_,Bot) -> 1
      | (Var a,Var b) -> compare a b
      | (Var _,_) -> -1
      | (_,Var _) -> 1
      | (Imp (a1,a2),Imp (b1,b2)) ->
          match compare a1 b1 with
            | 0 -> compare a2 b2
            | x -> x
  end

let pp_print_formula fmtr f =
  Format.pp_print_string fmtr (string_of_formula f)

module Assumptions = Set.Make(Formulas)
type theorem = Assumptions.t * formula(* = TODO: tu wpisz swoją definicję *)

let assumptions thm = thm |> fst |> Assumptions.elements

let consequence = snd

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
  Assumptions.singleton f,f

let imp_i f (ass,conc) = Assumptions.remove f ass , Imp(f,conc)


let imp_e (ass1,conc1) (ass2,conc2) =
  match conc1 with
  | Bot | Var _ -> failwith "Zla regula"
  | Imp(a,b) -> 
      if a = conc2 = 0
      then Assumptions.union ass1 ass2, b
      else failwith "Nieprawda"

let bot_e f (ass,conc) =
  match conc with
  | Bot -> ass,f
  | _ -> failwith "Nieprawda"
  
  

let fp = Var "p"
let fq = Var "q"
let fr = Var "r"
let fpq = Imp (fp,fq)
let fpqr = Imp(fp,Imp(fq,fr))
let fpr = Imp(fp,fr)

let p = by_assumption fp
let pp = imp_i fp p

let pqp = imp_i fp (imp_i fq p)

let pqrpqpr = imp_e (imp_e (by_assumption fpqr) p) (imp_e (by_assumption fpq) p) |> imp_i fp |> imp_i fpq |> imp_i fpqr 

let b_p = by_assumption Bot |> bot_e fp |> imp_i Bot 



let fp__ = (Imp(Imp(fp,Bot),Bot))
let p__ = by_assumption fp__
let fp_ = (Imp(fp,Bot))
let p_p = by_assumption fp_ |> imp_e p__ |> bot_e fp |> imp_i fp_
let fp_pp = (Imp(Imp(Imp(fp,Bot),fp),fp))
let p_pp = by_assumption fp_pp
let p_ppp__p = imp_e p_pp p_p |> imp_i fp__ |> imp_i fp_pp

