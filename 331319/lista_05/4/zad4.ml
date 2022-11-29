type 'a dllist = 'a dllist_data lazy_t
and 'a dllist_data = 
{
  prev : 'a dllist ;
  elem : 'a        ;
  next : 'a dllist 
}

let prev dxs = (Lazy.force dxs).prev 
let elem dxs = (Lazy.force dxs).elem 
let next dxs = (Lazy.force dxs).next 

let of_list_f f (xs, ys) = 
  match xs, ys with 
  | [], hd :: tl -> let r :: rs = List.rev ys in
    lazy {
      prev = f (rs, [r]); 
      elem = hd; 
      next = f (hd :: xs, tl)
    }
  | hd :: tl, [y] ->
    lazy {
      prev = f (tl, [hd; y]); 
      elem = y; 
      next =  f ([], List.rev (y :: xs))
    }
  | x :: xs, y :: ys ->
    lazy {
      prev = f (xs, x :: y :: ys);
      elem = y;
      next = f (y :: x :: xs, ys)
    }
  | _ -> failwith "of_list: fail"
;;

let of_list xs = Zad1.fix_memo of_list_f ([], xs) 

let test dxs n = 
  let rec go dxs i forward = 
    if i = 0 then true
    else if dxs == prev (next dxs) && dxs == next (prev dxs) then
      (go (if forward then next dxs else prev dxs) (i - 1) forward)
    else false
  in go dxs n true && go dxs n false 
;;
