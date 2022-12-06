let rec fold_left_cps (f : 'a -> 'b -> ('a -> 'c) -> 'c) (acc : 'a) (l : 'b list) (k : 'a -> 'c) : 'c =
    match l with
    | [] -> k acc
    | x::xs -> f acc x (fun a -> fold_left_cps f a xs k)

let id x = x

let for_all pred l =
  fold_left_cps (fun () x k -> pred x && k ()) () l (fun () -> true)

(* let l = List.init 10000000 (fun x -> false);; *)
(* for_all (fun x -> x = true) l;; *)

let mult_list l =
  fold_left_cps (fun () x k -> x * k ()) () l (fun () -> 1)


let sorted l =
  snd (fold_left_cps
         (fun () a k -> (Some a,
                          fun () ->
                          match k () with
                          | (None, k) -> k ()
                          | (Some b, k) -> a < b && k ()))
         () l (fun () -> (None, fun () -> true))) ()
