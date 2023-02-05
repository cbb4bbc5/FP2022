type 'a recc = In of ('a recc -> 'a)
let out (In x) = x

let fix f arg = (fun x a -> f (out x x) a) (In (fun x a -> f (out x x) a)) arg

let fix_mu f x = 
    let x = f x x in
    f x x
