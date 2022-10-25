(* fold left (((acc o 1) o 2) o 3) ...
   fold right ... n-3 o (n-2 o (n-1 o (n o acc))) *)

let length lst = List.fold_left (fun acc elem -> acc + 1) 0 lst ;;
let rev lst = List.fold_left (fun acc elem -> elem :: acc) [] lst ;;
let map lst f = List.fold_right (fun elem acc -> (f elem) :: acc) lst [] ;;
let append lst1 lst2 = List.fold_right (fun elem acc -> elem :: acc) lst1 lst2 ;;
let rev_append lst1 lst2 = List.fold_left (fun acc elem -> elem :: acc) lst2 lst1 ;;
let filter pred lst = List.fold_right (fun elem acc -> if (pred elem) then elem :: acc else acc) lst [] ;; 
let rev_map f lst = List.fold_left (fun acc elem -> (f elem) :: acc) [] lst ;;
