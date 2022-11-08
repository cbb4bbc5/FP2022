open Logic

let p = Var "p"
let q = Var "q"
let r = Var "r"

let theorem1 = imp_i p (by_assumption p)

let theorem2 = imp_i p (imp_i q (by_assumption p))

let theorem3 =
  let p_p = by_assumption p in
  let pq_pq = by_assumption (Imp(p, q)) in
  let pqp_q = imp_e pq_pq p_p in
  let pqr_pqr = by_assumption (Imp(p, Imp(q, r))) in
  let pqrp_qr = imp_e pqr_pqr p_p in
  let pqrpqp_r = imp_e pqrp_qr pqp_q in
  let pqrpq_pr = imp_i p pqrpqp_r in
  let pqr_pqpr = imp_i (Imp(p, q)) pqrpq_pr in
  let _pqrpqpr = imp_i (Imp(p, (Imp(q, r)))) pqr_pqpr
  in _pqrpqpr

let theorem4 = by_assumption Bot |> bot_e p |> imp_i Bot
