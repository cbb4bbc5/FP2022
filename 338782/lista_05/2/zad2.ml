type 'a rectype = {frec : 'a rectype -> 'a}
let fix1 : ((('a -> 'b) -> ('a -> 'b)) -> ('a -> 'b)) rectype -> (('a -> 'b) -> ('a -> 'b)) -> ('a -> 'b) = fun x f a -> f (x.frec x f) a
let fix_rectype f x = fix1 {frec = fix1} f x

let fix_state g = let f h = h:=g !h;fun x -> !h x in let f h x = f h x in g (f (ref (fun _ -> failwith""))) ;; 
