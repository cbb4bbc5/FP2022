type 'a rectype = {frec : 'a rectype -> 'a}
let fix1 : ((('a -> 'b) -> ('a -> 'b)) -> ('a -> 'b)) rectype -> (('a -> 'b) -> ('a -> 'b)) -> ('a -> 'b) = fun x f a -> f (x.frec x f) a
let fix_rectype f x = fix1 {frec = fix1} f x

let fix_state f0 x0 = 
    let fix = ref (fun f x -> f (fun _ -> failwith"") x)  in
    let f h x = h (!fix h) x
    in fix:=f;!fix f0 x0
