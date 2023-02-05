open Seq

type empty = |

type _ fin_type =
| Unit : unit fin_type
| Bool : bool fin_type
| Pair : ’a fin_type * ’b fin_type -> (’a * ’b) fin_type

let rec all_values: type a. a fin_type -> a Seq.t =
    fun t ->
    match t with
    | Unit -> Seq.return ()
    | Bool -> Seq.cons true (Seq.return false)
    | Pair(p, q) ->  Seq.product (all_values p) (all_values q)


(* let all_values (t : 'a fin_type) =
    let rec product xs ys =
        match xs with
        | h::tl -> List.append (List.map (fun x -> h::x) ys ) (product tl ys)
        | [] -> [ys] in
    let rec aux x =
        match x with
        | Unit -> [[()]]
        | Bool -> [[true]; [false]]
        | Pair(P1, P2) -> product (aux P1) (aux P2) in
    aux t *)
