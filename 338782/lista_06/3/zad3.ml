
exception EndFold

let for_all : ('a -> bool) -> 'a list -> bool =
  fun f xs ->
    try
      List.fold_left (fun e x -> if f x then true else raise EndFold) true xs
    with EndFold -> false

let mult_list : int list -> int =
  fun xs -> 
    try
      List.fold_left (fun e x -> if x<>0 then e*x else raise EndFold) 1 xs
    with EndFold -> 0

let sorted : int list -> bool =
  fun xs ->
    try
      let _ = List.fold_left (fun e x -> if e<=x then x else raise EndFold) (List.hd xs) xs in 
      true
    with EndFold -> false
