let rec sublists xs = List.fold_right ( fun x tail -> tail @ List.map (fun ys -> x :: ys) tail ) xs [[]]
