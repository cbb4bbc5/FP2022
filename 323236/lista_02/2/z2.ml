open List;;
let rec sublists s = 
  match s with
  | [] -> [[]]
  | (hd :: tl) -> let s = sublists tl in
    map (fun x -> hd :: x) s @ s;;
    
