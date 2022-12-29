type symbol = string
type 'v term =
| Var of 'v
| Sym of symbol * 'v term list


let rec bind m f = 
    match m with
    | Var v -> f v
    | Sym(s, vtlist) -> Sym(s, List.map (fun x -> bind x f) vtlist)


let return v = Var v
