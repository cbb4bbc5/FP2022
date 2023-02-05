open Logic

let t1 = 
  (by_assumption (Variable "p"))
  |> imp_i (Variable "p");;

let t2 = 
  by_assumption (Variable "p")
  |> imp_i (Variable "q")
  |> imp_i (Variable "p");;

let t3 = 
  imp_i 
  (Implication (Variable "p", Implication(Variable "q", Variable "r")))
  (imp_i 
        (Implication(Variable "p", Variable "q"))
        (imp_i (Variable "p")
                (imp_e (imp_e (by_assumption (Implication(Variable "p", Implication(Variable "q", Variable "r")))) (by_assumption (Variable "p")))
                (imp_e (by_assumption (Implication(Variable "p", Variable "q"))) (by_assumption (Variable "p"))))));;

let t4 = 
  (by_assumption False)
  |> bot_e (Variable "p")
  |> imp_i False;;
