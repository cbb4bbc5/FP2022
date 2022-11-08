let by_assumption f = Thm([f], f)

let imp_i f thm = 
  if List.mem f (assumptions thm)
  then Thm(List.filter (fun x -> x <> f) (assumptions thm), Impl(f,consequence thm))
  else failwith "Formula not in assumptions"

let imp_e th1 th2 = 
  match consequence th1, consequence th2 with
  | Impl(fi1,psi), fi2 when fi1 = fi2 -> 
      Thm(List.fold_left (fun acc x -> x::acc) (assumptions th1) (assumptions th2), psi)
  | _ -> failwith "Theorems break imp_e rule"

let bot_e f thm = 
  match thm with
    | Thm(assumptions,Bottom) -> Thm(assumptions, f)
    | _ -> failwith "Theorem consequence isn't a Bottom" 
