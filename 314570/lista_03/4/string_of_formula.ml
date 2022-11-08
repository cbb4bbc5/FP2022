let string_of_formula f =
  let rec string_of_formula_rec f =
    match f with
    | Bottom -> "⊥"
    | Var name -> name 
    | Impl(left, right) -> 
        ((match left with
         | Impl (_,_) -> "("^ (string_of_formula_rec left) ^")"
         | _ -> string_of_formula_rec left)
        ^ " → "^ string_of_formula_rec right) 
    in string_of_formula_rec f
