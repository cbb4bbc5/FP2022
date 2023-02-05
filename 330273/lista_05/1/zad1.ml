let rec fix f x = f (fix f) x

let fix_with_limit depth f x =
    let rec aux acc f x = if acc = depth then failwith "err" else f (aux (acc+1) f) x in
    aux 1 f x;;

(*
let memoize m f =
  let memo_table = Hashtbl.create m in
  (fun x ->
     match Hashtbl.find_opt memo_table x with
     | None -> Hashtbl.add memo_table x (f x); (f x)
     | Some x -> x )
*)

let fix_memo = 
    let memoize m f =
        let memo_table = Hashtbl.create m in
        (fun x ->
            match Hashtbl.find_opt memo_table x with
            | None -> Hashtbl.add memo_table x (f x); (f x)
            | Some x -> x ) in
    memoize 7321 fix

(*
let rec fix_memo f = 
    let memo_table = ref (Hashtbl.create 2137) in
    (fun x ->
        match Hashtbl.find_opt !memo_table x with
            | None -> 
                    let t = f (fix_memo f) x in
                    Hashtbl.add !memo_table x t; t
            | Some t -> t )
*)

