type 'a regexp =
    | Eps
    | Lit  of ('a -> bool)
    | Or   of 'a regexp * 'a regexp
    | Cat  of 'a regexp * 'a regexp
    | Star of 'a regexp

let ( +% ) r1 r2 = Or(r1,r2)
let ( *% ) r1 r2 = Cat(r1,r2)

open Zad4
let (let* ) = BT.bind
let return = BT.return
let fail = BT.fail
let flip = BT.flip
let run = BT.run

let rec match_regexp : 'a regexp -> 'a list -> 'a list option BT.t = 
    fun exp xs -> 
        match exp with
        | Eps -> return None
        | Lit f -> 
            begin match xs with
            | [] -> fail
            | x::xs ->
                if f x
                    then return (Some xs)
                    else fail
            end
        | Or(l,r) -> 
            let* dir = flip in
            if dir 
                then match_regexp l xs
                else match_regexp r xs
        | Cat(l,r) ->
            let* left=match_regexp l xs in
            begin match left with
                | None -> match_regexp r xs
                | Some xs -> match_regexp r xs
            end

        | Star exp1 ->
            let* leave = flip in
            if leave then return None
            else
                let* fit = match_regexp exp1 xs in
                begin match fit with
                    | None -> fail
                    | Some xs1 -> 
                        let* fit1 = match_regexp exp xs1 in
                        begin match fit1 with
                            | None -> return (Some xs1)
                            | Some _ -> return fit1
                        end
                end
            






