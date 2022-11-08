type formula = 
  | Bottom 
  | Var of string
  | Impl of formula * formula
