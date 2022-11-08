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
  module MyMap = Map.Make(Key)

  type key = Key.t
  type t = {
    perm : Key.t MyMap.t;
    inv  : Key.t MyMap.t
  }

  let apply t key =
    match MyMap.find_opt key t.perm with
    | None   -> key
    | Some v -> v

  let id = { perm = MyMap.empty; inv = MyMap.empty }

  let invert t = { perm = t.inv; inv = t.perm }

  let swap k l =
    let map = if Key.compare k l = 0
      then MyMap.empty
      else MyMap.singleton k l |> MyMap.add l k
    in { perm = map; inv = map }

  let compose m n =
    let non_id k v = Key.compare k v != 0
    in {
      perm = MyMap.map (apply m) n.perm |> MyMap.filter non_id;
      inv  = MyMap.map (apply (invert n)) m.inv |> MyMap.filter non_id
    }

  let compose m n =
    let wtf key opt_nv opt_mv =
      match opt_nv, opt_mv with
      | None, opt_mv -> opt_mv
      | Some nv, _   -> begin
        match apply m nv with
        | mnv when mnv = key -> None
        | mnv -> Some mnv
        end
    in {
      perm = MyMap.merge wtf n.perm m.perm;
      inv  = MyMap.merge wtf m.inv  n.inv
    }

  let compare m n = MyMap.compare Key.compare m.perm n.perm

end
