module type RandomMonad = sig
    type 'a t
    val return : 'a -> 'a t
    val bind : 'a t -> ('a -> 'b t) -> 'b t
    val random : int t
end

module RS : sig 
    include RandomMonad
    val run     : int -> 'a t -> 'a
end 
=
struct
    type 'a t = int -> 'a * int

    let calc_rand seed = 16807 * (seed mod 127773) - 2836 * (seed / 127773)

    let run seed monad = fst (monad seed)

    let return num = fun seed -> (num, seed) 

    let bind m f seed = 
        let (rand, new_seed) = m seed in 
            f rand new_seed

    let random seed = 
        let rand = calc_rand seed in 
            let rand = if rand > 0 then rand else rand + 2147483647 in
                (rand, rand) 
end
