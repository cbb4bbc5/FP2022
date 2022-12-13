open Logic

type context =
    | Root
    | Context of context * proof

type zipper = context * proof

type proof =
    | Proven of theorem
    | Imp_i  of proof * (string * formula) list * formula * zipper
    | Imp_e  of proof * proof * (string * formula) list * formula * zipper
    | Bot_e  of proof * (string * formula) list * formula * zipper
    | Hole   of (string * formula) list * formula

let proof g f =
    Hole(g, f)

let qed pf =
    let has_holes? pf =
        match pf with 
        | Proven(_) -> false
        | Imp_i(pf,_,_,_) -> has_holes? pf
        | Imp_e(p1,p2,_,_,_) -> (has_holes? p1) || (has_holes? p2)
        | Bot_e(pf,_,_,_) -> has_holes? pf
        | Hole(_,_) -> true in
    if has_holes? pf then failwith "the proof is incomplete" else
    in match pf with
    | Proven(thm) -> thm
    | Imp_i(_,g,f,_)   -> { assumptions = List.map (fun x -> snd x) g; consequence = f }
    | Imp_e(_,_,g,f,_) -> { assumptions = List.map (fun x -> snd x) g; consequence = f }
    | Bot_e(_,g,f,_)   -> { assumptions = List.map (fun x -> snd x) g; consequence = f }

let goal pf =
    match pf with
    | Proven(_) -> failwith "already proven"
    | Imp_i(_,_,_, (ctx, hole)) -> 
            match hole with
            | Hole(g, f) -> (g, f)
            | _ -> failwith "err"
    | Imp_e(_,_,_,_,(ctx, hole)) ->
            match hole with
            | Hole(g, f) -> (g, f)
            | _ -> failwith "err"
    | Bot_e(_,_,_,(ctx, hole)) ->
            match hole with
            | Hole(g, f) -> (g, f)    
            | _ -> failwith "err"
    | Hole(g, f) -> (g, f)

let go_up (ctx, pf) =
    match ctx with
    | Root -> pf
    | Context(ctx, pf) -> (ctx, pf)

let next pf = 
    let rec search_up (ctx, pf) prev =
        if pf = prev then (Context(ctx, pf), Proved({ assumptions = []; consequence = False }))
        match pf with
        | Imp_e (p1,p2,_,_,_) ->
            if p1 = prev then
                match search_down (Context(ctx, pf), p2) with
                | (_, Proved(_)) -> search_up (go_up (ctx, pf)) pf
                | (ctx, Hole(g,f)) -> (ctx, Hole(g,f))
            else
                match search_down (Context(ctx, pf), p1) with
                | (_, Proved(_)) -> search_up (go_up (ctx, pf)) pf
                | (ctx, Hole(g,f)) -> (ctx, Hole(g,f))
        | _ -> search_up (go_up (ctx, pf)) pf
    let rec search_down (ctx, pf) =
        match pf with
        | Proven(thm) -> (Root, Proven(thm))
        | Imp_i (p,_,_,_)   -> search_down (Context(ctx, pf), p)
        | Imp_e (p1,p2,_,_,_) -> 
                match search_down (Context(ctx, pf) p1)  with
                | (Root, Proven(thm)) -> 
                        match search_down (Context(ctx, pf) p2) with
                        | (Root, Proved(thm)) -> search_up (go_up (ctx, pf)) pf
                        | (ctx, hole) -> (Contex(ctx, pf), hole)
                | (ctx, hole) -> (Context(ctx, pf), hole) 
                     (* match search_down (Context(ctx, pf), p1) with
                        | (Root, Proved(thm)) -> search_up (go_up (ctx, pf)) pf
                        | _ -> search_down (Context(ctx, pf) p1) *)
        | Bot_e (p,_,_,_) -> search_down (Context(ctx, pf), p)
        | Hole(g, f) -> (Context(ctx, pf), Hole(g, h))
     match pf with
     | Proven(_) -> failwith "nothing to prove"
     | Imp_i(p,g,f,(ctx, hole))->
            match search_up (go_up (ctx, hole)) hole with
            | (Root, Proven(_))-> Proven({ assumptions = g, consequence = f })
            | (ctx, Proven(_)) -> Imp_i(p,g,f,(ctx, hole))
            | (ctx, hole)      -> Imp_i(p,g,f,(ctx, hole))
     | Imp_e(p1,p2,g,f,(ctx, hole)) ->
            match search_up (go_up (ctx, hole)) hole with
            | (Root, Proven(_))-> Proven({ assumptions = g, consequence = f })
            | (ctx, Proven(_)) -> Imp_e(p1,p2,g,f,(ctx, hole))
            | (ctx, hole)      -> Imp_e(p1,p2,g,f,(ctx, hole))
     | Bot_e(p,g,f,(ctx, hole))->
            match search_up (go_up (ctx, hole)) hole with
            | (Root, Proven(_))-> Proven({ assumptions = g, consequence = f })
            | (ctx, Proven(_)) -> Bot_e(p,g,f,(ctx, hole))
            | (ctx, hole)      -> Bot_e(p,g,f,(ctx, hole))
     | Hole(g, f) -> Hole(g, f)


let intro name pf =

    let rec insert pf imp_i goal v =
        if pf = v then
            match pf with
            | Imp_i(_,g,f,z) -> Imp_i(imp_i,g,f,z)
            | Imp_e(p1,p2,g,f,z) ->
                    if p1 = goal then Imp_e(imp_i,p2,g,f,z)
                    else Imp_e(p1,imp_i,g,f,z)
            | Bot_i(_,g,f,z) -> Bot_i(imp_i,g,f,z)
            | _ -> failwith "err"
        else match pf with
        | Proven(thm) -> Proven(thm)
        | Imp_i(p,g,f,z) -> Imp_i(insert p imp_i goal v,g,f,z)
        | Imp_e(p1,p2,g,f,z) -> Imp_e(insert p1 imp_i goal v, insert p2 imp_i goal v,g,f,z)
        | Bot_i(p,g,f,z) -> Imp_e(insert p imp_i goal v,g,f,z)
        | Hole(g,h) -> Hole(g,h) in
        
    match pf with
    | Proven(_) -> failwith "err"
    | Imp_i(_,_,_,(ctx, hole)) ->
            match hole with
            | Hole(g,f) ->
                    match f with
                    | Implication(a,b) ->
                        let t = insert pf Imp_i(Hole((name,a)::g,b),g,f,Hole([],False)) hole (go_up (ctx,hole)) in 
                        match t with
                            | Imp_i(p,g,f,_) -> Imp_i(p,g,f,next t)
                            | Proven(thm) -> Proven(thm)
                            | Hole(g,h) -> Hole(g,h)
                    | _ -> failwith "err"
            | _ -> failwith "err"

    | Imp_e(_,_,_,_,(ctx, hole)) ->
            match hole with
            | Hole(g,f) ->
                    match f with
                    | Implication(a,b) ->
                        let t = insert pf Imp_i(Hole((name,a)::g,b),g,f,Hole([],False)) hole (go_up (ctx,hole)) in 
                        match t with
                            | Imp_i(p,g,f,_) -> Imp_i(p,g,f,next t)
                            | Proven(thm) -> Proven(thm)
                            | Hole(g,h) -> Hole(g,h)
                    | _ -> failwith "err"
            | _ -> failwith "err"

    | Bot_i(_,_,_,(ctx, hole)) ->
         match ctx with
            match hole with
            | Hole(g,f) ->
                    match f with
                    | Implication(a,b) ->
                        let t = insert pf Imp_i(Hole((name,a)::g,b),g,f,Hole([],False)) hole (go_up (ctx,hole)) in 
                        match t with
                            | Imp_i(p,g,f,_) -> Imp_i(p,g,f,next t)
                            | Proven(thm) -> Proven(thm)
                            | Hole(g,h) -> Hole(g,h)
                    | _ -> failwith "err"
            | _ -> failwith "err"

    | Hole(g,f) ->
                    match f with
                    | Implication(a,b) ->
                        let t = Imp_i(Hole((name,a)::g,b),g,f,Hole([],False)) in
                        match t with
                            | Imp_i(p,g,f,_) -> Imp_i(p,g,f,next t)
                            | Proven(thm) -> Proven(thm)
                            | Hole(g,h) -> Hole(g,h)
                    | _ -> failwith "err"
 


(*
let apply f pf =
    match pf with
    | Proven(_) -> failwith "err"
    | Imp_i(_,_,_,(ctx, hole)) ->


let apply_thm thm pf =
  (* TODO: zaimplementuj *)
  failwith "not implemented"

let apply_assm name pf =
  (* TODO: zaimplementuj *)
  failwith "not implemented"

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
*)

