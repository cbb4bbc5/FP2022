let rec merge cmp xs ys = 
  match xs with
  | [] -> ys
  | x :: xs as l1 -> 
    begin match ys with
    | [] -> l1
    | y :: ys as l2 -> 
      if cmp x y then x :: (merge cmp xs l2)
      else y :: (merge cmp l1 ys)
    end
  ;;
;;

let merge_tail cmp xs ys = 
  let rec go cmp xs ys acc = 
    match xs with 
    | [] -> List.rev_append acc ys
    | x :: xs as l1 ->
      begin match ys with
      | [] -> List.rev_append acc l1
      | y :: ys as l2 ->
        if cmp x y then go cmp xs l2 (x :: acc)
        else go cmp l1 ys (y :: acc)
      end
  in go cmp xs ys []
;;
(* Wersja nieogonowa doprowadza do przepełnienia stosu
   przy sortowaniu dziewięćdziesiąttysięcyelementowych list,
   podczas gdy wersja ogonowa nie ma tego problemu.
   Wersja ogonowa jest jednak odrobinę wolniejsza. *)

let halve xs = 
  let rec go xs (h1, h2) = 
    match h1 with 
    | [] | [_] -> (xs, List.rev h2)
    | h :: g :: h1 -> go (List.tl xs) (h1, (List.hd xs) :: h2)
  in go xs (xs, [])
;;

let rec mergesort cmp xs = 
  match xs with
  | [] | [_] -> xs 
  | _ :: _ :: _ -> 
    let p = halve xs in
     merge cmp (mergesort cmp (fst p)) (mergesort cmp (snd p))
  ;;
;;
