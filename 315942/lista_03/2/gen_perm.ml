module type PermutationType = sig
    type t
    val id : t
    val invert : t -> t
    val compose : t -> t -> t
    val compare : t -> t -> int
end

module type S = sig
    type per
    
    (** Sprawdza, czy dana permutacja jest generowana przez daną listę permutacji *)
    val is_generated : per -> per list -> bool
end

module Make(Per : PermutationType) = struct
    module Set = Set.Make(Per)

    type per = Per.t (* apply, id, invert, swap, compose, compare *)

    let is_generated p (qs : per list) =
        let rec set_n xs =
            let ys = Set.union xs (Set.union (Set.fold (fun x ys ->
                Set.add (Per.invert x) ys
            ) xs Set.empty) (List.fold_left Set.union Set.empty (Set.fold (fun x ys -> 
                (Set.fold (fun y zs ->
                    Set.add (Per.compose x y) zs
                ) xs Set.empty) :: ys
            ) xs [])))
            in if Set.exists (fun x -> 
                   if 0 = compare x p 
                   then true else false) ys
               then true 
               else if 0 = compare xs ys
                   then false
                   else set_n ys
        in set_n (List.fold_left (fun ys x -> 
            Set.add x ys
        ) Set.empty (Per.id :: qs))
end