type 'a clist = { clist : 'z. ('a -> 'z -> 'z) -> 'z -> 'z }

let cnil = { clist = fun cons z -> z }

let ccons a xs = { clist = fun cons ys -> cons a (xs.clist cons ys) }

let map f xs = { clist = fun cons ys -> xs.clist (fun a -> cons (f a)) ys }

let append xs ys = { clist = fun cons zs -> xs.clist cons (ys.clist cons zs) }

let clist_to_list xs = xs.clist (fun x xs -> x :: xs) []

let rec clist_of_list xs =
  match xs with
  | []      -> cnil
  | x :: xs -> ccons x (clist_of_list xs)

let rec clist_of_list2 xs = List.fold_right ccons xs cnil

let prod xs ys = { clist = fun cons zs -> xs.clist (fun a -> (ys.clist (fun b -> cons (a, b)))) zs }
