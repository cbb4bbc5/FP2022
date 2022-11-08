open Logic;;
let p = Var("p")
let q = Var("q")
let r = Var("r")
let t__p = by_assumption p

let proof1 = imp_i p t__p

let proof2 =  imp_i p (imp_i q t__p)  
let proof4 = imp_i Bot (bot_e p (by_assumption Bot)) 

let f__p_q = Imp(p,q)
let f__p_q_r = Imp(p,Imp(q,r))

let t__q_r =
   imp_e (by_assumption f__p_q_r) t__p
let t__p_q = by_assumption (Imp(p,q))
let t__r = imp_e t__q_r (imp_e t__p_q t__p)

let proof3 =
  imp_i f__p_q_r (imp_i f__p_q (imp_i p t__r))
