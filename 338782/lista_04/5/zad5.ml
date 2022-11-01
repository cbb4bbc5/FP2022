
let intro name = function
  | Complete thm -> Complete thm
  | Incomplete(Goal (ass,Imp(a,b)),cont) -> Incomplete(Goal ((name,a)::ass,b),CImpI(a,cont))
  | _ -> failwith "Not Imp"

