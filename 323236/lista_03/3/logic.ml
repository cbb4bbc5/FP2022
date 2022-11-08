
(* ------------>  zadanie 3 *)

type formula =
  | Bot
  | Var of string
  | Imp of formula * formula

(* ------------>  zadanie 4 *)

let string_of_formula f =
  (* TODO: zaimplementuj *)
  let rec convert f brackets =
    match f with
    | Bot -> "⊥"
    | Var(x) -> x
    | Imp(f1,f2 ) -> if brackets
                    then "(" ^ convert f1 true ^ " ⇒ " ^ convert f2 false ^ ")"
                    else convert f1 true ^ " ⇒ " ^ convert f2 false
  in convert f false


let pp_print_formula fmtr f =
  Format.pp_print_string fmtr (string_of_formula f)


(* ------------>  zadanie 5 *)
module Formula =
struct
  type t = formula
  let compare = compare
end

module Assumptions = Set.Make(Formula)


type theorem = Theorem of Assumptions.t * formula

let assumptions = function Theorem(assumed,_) ->
  Assumptions.fold (fun x xs -> x :: xs) assumed []
let consequence = function Theorem(_, thesis) -> thesis

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
  (* TODO: zaimplementuj *)
  Theorem(Assumptions.singleton f,f)

let imp_i f thm =
  (* TODO: zaimplementuj *)
  match thm with
  | Theorem(assumed, thesis) ->
      Theorem(Assumptions.remove f assumed, Imp(f,thesis))
let imp_e th1 th2 =
  (* TODO: zaimplementuj *)
  match th1, th2 with
  | Theorem(a1,Imp(psi1,phi)), Theorem(a2,psi2) when psi1 = psi2 ->
    Theorem(Assumptions.union a1 a2, phi)
  | _ -> failwith "imp_e fail"
let bot_e f thm =
  (* TODO: zaimplementuj *)
  match thm with
  | Theorem(assumed, Bot) -> Theorem(assumed, f)
  | _ -> failwith "bot_e fail"

  let p = Var("p")
  let q = Var("q")
  let r = Var("r")
  let t__p = by_assumption p

  let proof1 = imp_i p t__p

  let proof2 =  imp_i p (imp_i q t__p)  
  let proof4 = imp_i Bot (bot_e p (by_assumption Bot)) 
  
  let f__p_q = Imp(p,q)
  let f__p_q_r = Imp(p,Imp(q,r))

  let t__q_r =
     imp_e (by_assumption f__p_q_r) t__p
  let t__p_q = by_assumption (Imp(p,q))
  let t__r = imp_e t__q_r (imp_e t__p_q t__p)

  let proof3 =
    imp_i f__p_q_r (imp_i f__p_q (imp_i p t__r))
