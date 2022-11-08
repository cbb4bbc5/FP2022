type formula =
  | Var of string
  | Bot 
  | Imp of formula * formula

let rec string_of_formula = function
  | Bot -> "⊥"
  | Var p -> p
  | Imp ((Imp _) as a,b) -> "("^string_of_formula a^") -> "^string_of_formula b
  | Imp (a,b) -> string_of_formula a^" -> "^string_of_formula b

