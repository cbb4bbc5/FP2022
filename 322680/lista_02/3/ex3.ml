let rec suffixes l =
  match l with
  | [] -> [[]]
  | h :: tail ->
    let suffs = suffixes tail
    in (h :: List.hd suffs) :: suffs
let suffixes l =
  List.fold_right (fun a b -> (a :: List.hd b) :: b) l [[]]

let prefixes l =
  List.fold_right (fun a b -> [] :: (List.map (fun l -> a :: l) b)) l [[]]
let prefixes l =
  List.fold_left (fun a b -> (List.append (List.hd a) [b]) :: a) [[]] l