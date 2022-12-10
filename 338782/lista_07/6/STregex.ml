open Zad6
module STRegex(TypeOf :sig type t end) = struct
    module TypedList(ListType:sig type t end) = 
        struct type t = ListType.t list end
    module BT = STB(TypedList(TypeOf))
    open BT
    type 'a regexp =
        | Eps
        | Lit  of ('a -> bool)
        | Or   of 'a regexp * 'a regexp
        | Cat  of 'a regexp * 'a regexp
        | Star of 'a regexp

    let ( +% ) r1 r2 = Or(r1,r2)
    let ( *% ) r1 r2 = Cat(r1,r2)

    let (let* ) = BT.bind 
    let rec match_regexp exp =
            match exp with
                | Eps -> return []
                | Lit f -> 
                    let* xs = get in
                    begin match xs with
                    | [] -> fail
                    | x::xs ->
                        if f x
                            then 
                                let* () = put xs in
                                return [x]
                            else fail
                    end
                | Or(l,r) -> 
                    let* dir = flip in
                    if dir
                        then match_regexp l
                        else match_regexp r
                | Cat(l,r) ->
                    let* left=match_regexp l in
                    let* right=match_regexp r in
                    return (left@right)
                | Star exp1 ->
                    let* leave = flip in
                    if leave then return []
                    else
                        let* fit = match_regexp exp1 in
                        begin match fit with
                            | [] -> fail
                            | xs1 -> 
                                let* fit1 = match_regexp exp in
                                begin match fit1 with
                                    | [] -> return xs1
                                    | xs2 -> return (xs1@xs2)
                                end
                        end
end
