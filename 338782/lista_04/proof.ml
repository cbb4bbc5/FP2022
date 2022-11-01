open Logic

type goal = (string * formula) list * formula

type prooftree = 
  | Proven of theorem
  | Goal of goal
  | ImpI of formula * prooftree
  | ImpE of prooftree * prooftree
  | BotE of formula * prooftree

type proofcontext = 
  | Top
  | CImpI of formula * proofcontext
  | CImpEF of prooftree * proofcontext
  | CImpEI of proofcontext * prooftree
  | CBotE of formula * proofcontext

type proof =
  | Complete of theorem
  | Incomplete of prooftree * proofcontext

let proof g f =
  Incomplete (Goal (g,f),Top)

let qed = function 
  | Complete thm -> thm
  | Incomplete _ -> failwith "Incomplete"

let goal = function
  | Complete _ -> None
  | Incomplete (Goal mp,_) -> Some mp
  | _ -> failwith "Not a goal"

let context_of_proof = function
  | Complete _ -> Top
  | Incomplete(_,cont) -> cont

let rec upright = function
  | (tree,Top) -> (tree,Top)
  | (tree,CImpI(f,cont)) -> upright (ImpI(f,tree),cont)
  | (tree,CBotE(f,cont)) -> upright (BotE(f,tree),cont)
  | (tree,CImpEF(imp,cont)) -> upright (ImpE(imp,tree),cont)
  | (tree,CImpEI(cont,Proven thm)) -> upright (ImpE(tree,Proven thm),cont)
  | (tree,CImpEI(cont,f)) -> (f,CImpEF(tree,cont))

let rec downleft = function
  | (Goal _,_) | (Proven _,_) as tree -> tree
  | (ImpI(f,tree),cont) -> downleft (tree,CImpI (f,cont))
  | (BotE(f,tree),cont) -> downleft (tree,CBotE (f,cont))
  | (ImpE(Proven thm,tree),cont) -> downleft (tree,CImpEF(Proven thm,cont))
  | (ImpE(tree,f),cont) -> downleft (tree,CImpEI(cont,f))

let pair_to_proof (tree,cont) = Incomplete(tree,cont)

let next = function
  | Complete thm -> Complete thm
  | Incomplete (tree,cont) -> 
    let rec complete (tree,cont) = 
        match tree with
        | Proven thm ->
          begin match cont with
              | Top -> Complete thm
              | CImpI (f,cont) -> complete (Proven (imp_i f thm),cont)
              | CBotE (f,cont) -> complete (Proven (bot_e f thm),cont)
              | CImpEF (Proven thm2,cont) -> complete (Proven (imp_e thm2 thm),cont)
              | CImpEF _ -> (tree,cont) |> upright |> downleft |> pair_to_proof
              | CImpEI (cont,Proven thm2) -> complete (Proven (imp_e thm thm2),cont)
              | CImpEI _ -> (tree,cont) |> upright |> downleft |> pair_to_proof
          end
        | _ -> (tree,cont) |> upright |> downleft |> pair_to_proof
    in complete (tree,cont)
      


let intro name = function
  | Complete thm -> Complete thm
  | Incomplete(Goal (ass,Imp(a,b)),cont) -> Incomplete(Goal ((name,a)::ass,b),CImpI(a,cont))
  | _ -> failwith "Not Imp"

let rec implist orig ?(acc=[]) f =
  match f with
  | Imp(a,f) when (Imp(a,f)) <> orig -> implist orig ~acc:(a::acc) f
  | _ when f = orig -> (true,false,acc)
  | Bot -> (true,true,acc)
  | _ -> (false,false,acc)

let rec expand_cont ass orig cont = function
  | [] -> Incomplete(Goal (ass,orig),cont)
  | (x::xs) -> expand_cont ass orig (CImpEI(cont,Goal (ass,x))) xs

let apply f = function
  | Complete thm -> Complete thm
  | Incomplete(Goal (ass,ph),cont) ->
    let (bthm,bbot,flist) = implist ph f in
    let cont = if bbot then CBotE(ph,cont) else cont in
      if bthm 
        then expand_cont ass f cont flist
        else failwith "Wrong theorem"
  | _ -> failwith "Not goal"

let apply_thm thm pf = 
  thm |> consequence |> Fun.flip apply pf |> (fun x -> Incomplete(Proven thm,context_of_proof x)) |> next


let apply_assm name pf = match pf |> goal with 
  | None -> pf
  | Some goal -> apply_thm (List.assoc name (goal |> fst) |> by_assumption) pf

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
