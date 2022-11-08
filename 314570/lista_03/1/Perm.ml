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

module Make(Key: OrderedType) = struct
  module PermMap = Map.Make(Key)

  type key = Key.t
  type t = key PermMap.t * key PermMap.t

  let apply perm x = 
    try PermMap.find x (fst perm) with
      Not_found -> x 

  let id = (PermMap.empty, PermMap.empty)

  let invert perm = ((snd perm), (fst perm))

  let swap key1 key2 = 
    if (Key.compare key1 key2) = 0
    then id 
    else let app = PermMap.add key1 key2 (PermMap.add key2 key1 PermMap.empty)
         in (app, app)

  let compose perm1 perm2 = 
    let function_compose f1 f2 =  
      PermMap.merge (fun key v1 v2 -> match v1, v2 with
                                      | Some v1, _ -> let app = try PermMap.find v1 f2 with Not_found -> v1
                                                      in if (Key.compare key app) = 0 then None else Some app
                                      | None, Some v2 -> Some v2
                                      | None, None -> None) 
                         f1 f2
    in (function_compose (fst perm1) (fst perm2), function_compose (snd perm2) (snd perm1))

  let compare perm1 perm2 = PermMap.compare Key.compare (fst perm1) (fst perm2)

end
