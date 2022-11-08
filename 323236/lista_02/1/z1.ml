open List;;

let length s =
  fold_left (fun x y -> x + 1) 0 s;;

let map f s =
  fold_right (fun x y -> (f x) :: y) s [];;
let rev s =
  fold_left (fun x y -> y :: x) [] s;;

let append s1 s2 =
  fold_right (fun x y -> x :: y) s1 s2;;

let rev_map f s =
  fold_left (fun x y -> (f y) :: x) [] s;;

let rev_append s1 s2 =
  append(rev s1) s2;;

let filter p s = 
  fold_right (fun x y -> if (p x) then x :: y else y) s [];;
  
let test_p x = if x > 0 then true else false;;

