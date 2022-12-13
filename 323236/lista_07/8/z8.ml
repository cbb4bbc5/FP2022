type symbol = string
type 'v term =
| Var of 'v
| Sym of symbol * 'v term list

let return a = Var a

let bind t f =
  let rec aux =
    function
    | Var a        -> f a
    | Sym(sym, xs) -> Sym(sym, List.map aux xs)
  in aux t
