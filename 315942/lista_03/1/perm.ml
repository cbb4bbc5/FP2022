module type OrderedType = sig
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

module Make(Key : OrderedType) = struct
    module Map = Map.Make(Key)

    type key = Key.t
    type t = {per : key Map.t; inv : key Map.t}

    let apply p k = try Map.find k p.per 
                    with Not_found -> k
    let id = {per = Map.empty; inv = Map.empty}
    let invert p = {per = p.inv; inv = p.per}
    let swap k l = if 0 <> compare k l 
                   then let map = Map.add k l (Map.add l k Map.empty) 
                        in {per = map; inv = map}
                   else id
    let compose p q = 
        let fmap k px qx = match px, qx with 
            | _, Some v -> begin match apply p v with
                | v when k <> v -> Some v 
                |             _ -> None end
            | Some v, _ -> Some v
            |         _ -> None (* by uspokoić kompilator *)
        in let map = Map.merge fmap p.per q.per
            in {per = map; inv = Map.fold (fun k px q -> 
                Map.add px k q
            ) map Map.empty}

    let compare p q = Map.compare (Key.compare) p.per q.per
end
(*
module M = Make(Int);;

let app = (M.apply)
and id  = (M.id)
and inv = (M.invert)
and swp = (M.swap)
and cps = (M.compose)
and cpr = (M.compare);;
let x = swp 1 2
and y = swp 2 3
and z = swp 3 4;;
let xy = cps x y
and xz = cps x z
and yz = cps y z;;
let yx = cps y x
and ixy = inv xy;;
let xyz = cps x (cps y z);;
let ixyz = inv xyz;;
*)