
type symbol = string
type 'v term =
| Var of 'v
| Sym of symbol * 'v term list

let return v = Var v
let rec bind m f = 
    match m with
    | Var v -> f v
    | Sym(s,xs) -> Sym(s,List.map (fun m -> bind m f) xs)

(* Podstawianie *)
