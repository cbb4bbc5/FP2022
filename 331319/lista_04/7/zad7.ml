open Logic 
open Proof 

let p = Var "p"
let q = Var "q"
let r = Var "r"

let proof_a = proof [] (Cond(Cond(p, Cond(q, r)), Cond(Cond(p, q), Cond(p, r))))
              |> intro "H1" |> intro "H2" |> intro "H3"  
              |> apply_assm "H1" |> apply_assm "H3"
              |> apply_assm "H2" |> apply_assm "H3" 
              |> qed
;;

let proof_b = proof [] (Cond(Cond(Cond(Cond(p, Spike), p), p), Cond(Cond(Cond(p, Spike), Spike), p)))
              |> intro "H1" |> intro "H2" 
              |> apply_assm "H1" 
              |> intro "H3" 
              |> apply_assm "H2" |> apply_assm "H3" 
              |> qed
;;

let proof_c = proof [] ((Cond(Cond(Cond(Cond(p, Spike), Spike), p), Cond(Cond(Cond(p, Spike), p), p))))
              |> intro "H1" |> intro "H2" 
              |> apply_assm "H1" 
              |> intro "H3" 
              |> apply_assm "H3" |> apply_assm "H2" |> apply_assm "H3" 
              |> qed
;;
