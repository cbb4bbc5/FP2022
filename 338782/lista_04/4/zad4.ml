
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
      

