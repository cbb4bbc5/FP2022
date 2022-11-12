
module type OrderedType = 
    sig
        type t
        val compare : t -> t -> int
    end

module type S = sig
    type key
    type t
    (** permutacja jako funkcja *)
    val apply : t -> key -> key
    (** permutacja identycznościowa *)
    val id : t
    (** permutacja odwrotna *)
    val invert : t -> t
    (** permutacja która tylko zamienia dwa elementy miejscami *)
    val swap : key -> key -> t
    (** złożenie permutacji (jako złożenie funkcji) *)
    val compose : t -> t -> t
    (** porównywanie permutacji *)
    val compare : t -> t -> int
end


module Make(Key : OrderedType) = 
    struct
        module Maps = Map.Make(Key)
        type key = Key.t
        type t = (key Maps.t * key Maps.t)
        let id = (Maps.empty,Maps.empty)
        let invert (x,y) = (y,x)
        let apply (m,_) x = match Maps.find_opt x m with
            | None -> x
            | Some x -> x
        let swap a b = let x = if Key.compare a b = 0 then fst id else Maps.empty |> Maps.add a b |> Maps.add b a in (x,x)
        let union k a b = 
            if Key.compare b k = 0 
            then None
            else Some b 
        let compose f g = (fst f |> Maps.map (apply g) |> Maps.union union (fst g),snd g |> Maps.map (apply (invert f)) |> Maps.union union (snd f))
        let compare (b,_) (a,_) = Maps.compare (Fun.flip Key.compare) a b
    end
