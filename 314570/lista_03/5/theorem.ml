type theorem = Thm of formula list * formula

let assumptions thm = match thm with Thm(assumptions, _) -> assumptions 
let consequence thm = match thm with Thm(_,consequence) -> consequence 
