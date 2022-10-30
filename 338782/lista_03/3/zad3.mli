type formula =
  | Var of string
  | Bot 
  | Imp of formula * formula
