open Logic 

let th1 = imp_i (Var "p") (by_assumption (Var "p"));;

let th2 = imp_i (Var "p") (imp_i (Var "q") (by_assumption (Var "p")));;

let helper1 = imp_e (by_assumption (Impl(Var "p", Var "q"))) (by_assumption (Var "p"))
let helper2 = imp_e (by_assumption (Impl(Var "p", Impl(Var "q", Var "r")))) (by_assumption (Var "p")) 
let th3 = imp_e helper2 helper1 |> imp_i (Var "p") |> imp_i (Impl(Var "p", Var "q")) |> imp_i (Impl(Var "p", Impl(Var "q", Var "r")));;

let th4 = imp_i (Bottom) (bot_e (Var "p") (by_assumption Bottom));;

