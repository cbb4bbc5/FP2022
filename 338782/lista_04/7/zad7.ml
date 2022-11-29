open Logic
open Proof

let pqrpqpr = proof [] (formula_of_string "(p -> q -> r) -> (p -> q) -> p -> r") |> intro "pqr" |> intro "pq" |> intro "p" |> apply_assm "pqr" |> apply_assm "p" |> apply_assm "pq" |> apply_assm "p" |> qed

let p_ppp__p = proof [] (formula_of_string "(((p -> _) -> p) -> p) -> ((p -> _) -> _) -> p") |> intro "p_pp" |> intro "p__" |> apply_assm "p_pp" |> intro "p_" |> apply_assm "p__" |> apply_assm "p_" |> qed

let p__pp_pp = proof [] (formula_of_string "(((p -> _) -> _) -> p) -> ((p -> _) -> p) -> p") |> intro "p__p" |> intro "p_p" |> apply_assm "p__p" |> intro "p_" |> apply_assm "p_" |> apply_assm "p_p" |> apply_assm "p_" |> qed

