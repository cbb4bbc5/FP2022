open Logic
open Proof

let p = Var "p"
let q = Var "q"
let r = Var "r"
let pq = Imp(p,q)
let pqr = Imp(p,Imp(q,r))
let pqrpqpr = Imp(pqr,Imp(pq,Imp(p,r)))
let p_ = Imp(p,Bot)
let p_pp = Imp(Imp(p_,p),p)
let p__ = Imp(p_,Bot)
let p_ppp__p = Imp(p_pp,Imp(p__,p))
let p__p = Imp(p__,p)
let p__pp_pp = Imp(p__p,Imp(Imp(p_,p),p))

let thm_pqrpqpr = proof [] pqrpqpr |> intro "pqr" |> intro "pq" |> intro "p" |> apply_assm "pqr" |> apply_assm "p" |> apply_assm "pq" |> apply_assm "p" |> qed

let thm_p_ppp__p = proof [] p_ppp__p |> intro "p_pp" |> intro "p__" |> apply_assm "p_ppa" |> intro "p_" |> apply_assm "p__" |> qed

let thm_p__pp_pp = proof [] p__pp_pp |> intro "p__p" |> intro "p_p" |> apply_assm "p__p" |> intro "p_" |> apply_assm "p_" |> apply_assm "p_p" |> apply_assm "p_" |> qed
