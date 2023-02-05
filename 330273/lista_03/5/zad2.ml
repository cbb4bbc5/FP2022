let is_generated (type perm) (module Perm : Perm.S with type t = perm) p xs =
  let module PermSet = Set.Make(Perm) in
  let rec aux curr = 
    let next =
      let inverted = PermSet.map Perm.invert curr in (* map mapuje używając funkcji invert *)
      let product =
        PermSet.fold
          (fun perm acc -> PermSet.map (Perm.compose perm) curr |> PermSet.union acc) (* e |> f ~ f e *)  
          curr
          PermSet.empty
      in PermSet.union curr (PermSet.union inverted product)
    in PermSet.mem p next || PermSet.compare curr next <> 0 && aux next
  in aux (PermSet.of_list xs |> PermSet.add Perm.id)

module IntPerm = Perm.Make(Int)
open IntPerm

let is_generated_int = is_generated (module IntPerm: Perm.S with type t = IntPerm.t)
