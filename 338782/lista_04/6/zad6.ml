
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

