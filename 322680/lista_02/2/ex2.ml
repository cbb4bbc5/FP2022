let rec sublists l =
  match l with
  | [] -> [[]]
  | h :: tail ->
    let sls = sublists tail
    in sls @ (List.map (fun l -> h :: l) sls)

let sublists l =
  List.fold_left (fun a b -> a @ (List.map (fun l -> b :: l) a)) [[]] l