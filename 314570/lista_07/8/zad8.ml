type symbol = string
type 'v term =
| Var of 'v
| Sym of symbol * 'v term list

let return : 'a -> 'a term = fun elem -> Var elem
let rec bind : 'a term -> ('a -> 'b term) -> 'b term = 
    fun term transform -> 
        match term with
        | Var term -> transform term 
        | Sym (symbol, terms) -> 
            Sym (symbol, List.map (fun term -> bind term transform) terms)

let term = Sym("+", [Var 5; Sym("-", [Var 10; Var 1])])


let calc = bind term (fun x -> return (2*x))
