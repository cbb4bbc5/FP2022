let rec fold_left_cps f acc xs k =
  match xs with
  | [] -> k acc
  | x::xs -> f acc x (fun r -> fold_left_cps f r xs k)

let for_all pred xs =
  fold_left_cps (fun acc x k -> 
                  match (pred x) with 
                  | true -> k true
                  | false -> false)
                true
                xs
                (fun x -> x)


let mult_list xs =
  fold_left_cps (fun acc x k ->
                  match x with
                  | 0 -> 0
                  | x -> k (x * acc))
                1
                xs
                (fun x -> x)

let sorted xs =
  fold_left_cps (fun last x k ->
                  match x > last with
                  | true -> k x 
                  | false -> false)
                (List.hd xs) 
                (List.tl xs)
                (fun x -> true) 

