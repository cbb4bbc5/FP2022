open Logic

let thm1 = 
  (by_assumption (Var "p"))
  |> imp_i (Var "p")
;;

let thm2 = 
  by_assumption (Var "p")
  |> imp_i (Var "q")
  |> imp_i (Var "p")
;;

let thm3 = 
  imp_i (Cond(Var "p", Cond(Var "q", Var "r")) )
  (imp_i (Cond(Var "p", Var "q"))
         (imp_i (Var "p")
                (imp_e (imp_e (by_assumption (Cond(Var "p", Cond(Var "q", Var "r")))) (by_assumption (Var "p")))
                       (imp_e (by_assumption (Cond(Var "p", Var "q"))) (by_assumption (Var "p"))))))
;;

let thm4 = 
  (by_assumption Spike)
  |> bot_e (Var "p")
  |> imp_i Spike
;;

let trudne = 
  imp_i (Cond(Cond(Cond(Var "p", Spike), Var "p"), Var "p"))
        (imp_i (Cond(Cond(Var "p", Spike), Spike))
                (imp_e (by_assumption (Cond(Cond(Cond(Var "p", Spike), Var "p"), Var "p")))
                        (imp_i (Cond(Var "p", Spike))
                                (bot_e (Var "p")
                                        (imp_e (by_assumption (Cond(Cond(Var "p", Spike), Spike)))
                                                (by_assumption (Cond(Var "p", Spike))))))))
;;
