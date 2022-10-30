module type S = 
    sig
        type t
        val is_generated : t -> t list -> bool
    end

module Make(Elt : Perm.S) = 
    struct
        type t = Elt.t
        module Sets = Set.Make(Elt)
        let is_generated x xs = let rec saturate xs =
            let xs1 = Sets.union (Sets.map Elt.invert xs) (Sets.fold (fun elt1 acc -> Sets.fold (fun elt2 acc -> Sets.add (Elt.compose elt1 elt2) acc) xs acc) xs Sets.empty)
            in if Sets.mem x xs1
                then true
                else if Sets.equal xs xs1
                    then false
                    else saturate xs1
            in saturate (Sets.of_list xs |> Sets.add Elt.id)

    end

let is_generated (type a) (module Elt : Perm.S with type t = a) x xs =
    let module Sets = Set.Make(Elt) in
    let rec saturate xs =
    let xs1 = Sets.union (Sets.map Elt.invert xs) (Sets.fold (fun elt1 acc -> Sets.fold (fun elt2 acc -> Sets.add (Elt.compose elt1 elt2) acc) xs acc) xs Sets.empty)
    in if Sets.mem x xs1
        then true
        else if Sets.equal xs xs1
            then false
            else saturate xs1
    in saturate (Sets.of_list xs |> Sets.add Elt.id)
