module type OrderedPermType = Perm.S

module type S = sig
  type perm
  val is_generated : perm -> perm list -> bool
end

module Make(Perm : OrderedPermType) = struct
  module MySet = Set.Make(Perm)

  type perm = Perm.t

  let saturation_iteration xs =
    let f perm acc =
      MySet.add (Perm.invert perm) acc
      |> MySet.union (MySet.map (Perm.compose perm) xs)
    in MySet.fold f xs xs

  let rec _is_generated p qs =
    MySet.mem p qs
    || let rs = saturation_iteration qs
        in MySet.compare qs rs != 0
           && _is_generated p rs

  let is_generated p g =
    let init = MySet.of_list (Perm.id :: g)
    in _is_generated p init

end
