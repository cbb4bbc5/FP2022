module type RandomMod = sig
    type 'a t
    val return : 'a -> 'a t
    val bind : 'a t -> ('a -> 'b t) -> 'b t
    val random : int t
    val run : int -> 'a t -> 'a
end

module RS : RandomMod = struct 
    type 'a t = int -> ('a * int)
    let next_seed seed = 
        let b = 16807 * (seed mod 127773) - 2836 * (seed / 127773) in
        if b>0 then b else b + 2147483647
    let return x s = (x,next_seed s)
    let bind m f s = 
        let (a1,s1) = m (next_seed s) in
        let (a2,s2) = f a1 (next_seed s1) in
        (a2,next_seed s2) 
    let random s = let a = next_seed s in (a,a)
    let run s m = fst (m s)
end

