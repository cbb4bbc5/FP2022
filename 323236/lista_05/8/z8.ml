type _ fin_type =
  | Unit : unit fin_type
  | Bool : bool fin_type
  | Pair : 'a fin_type * 'b fin_type -> ('a * 'b) fin_type


  let rec all_values : type a. a fin_type -> a Seq.t =
    function x -> match x with
    | Unit         -> Seq.return ()
    | Bool         -> List.to_seq [true; false]
    | Pair(ft1, ft2) ->   let ft2_mult = (fun x -> Seq.map (fun y -> x, y) (all_values ft2))
                        in Seq.flat_map
                        ft2_mult
                        (all_values ft1)
