open Logic

type goal = (string * formula) list * formula

type prooftree = 
  | Proven of theorem
  | Goal of goal
  | ImpI of formula * prooftree
  | ImpE of prooftree * prooftree
  | BotE of formula * prooftree

type proofcontext = 
  | Top
  | CImpI of formula * proofcontext
  | CImpEF of prooftree * proofcontext
  | CImpEI of proofcontext * prooftree
  | CBotE of formula * proofcontext

type proof =
  | Complete of theorem
  | Incomplete of prooftree * proofcontext

let proof g f =
  Incomplete (Goal (g,f),Top)

let qed = function 
  | Complete thm -> thm
  | Incomplete _ -> failwith "Incomplete"

let goal = function
  | Complete _ -> None
  | Incomplete (Goal mp,_) -> Some mp
  | _ -> failwith "Not a goal"


