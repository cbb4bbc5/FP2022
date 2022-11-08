module type PermType = sig
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

module type S = sig
  type key
  type t
  (** sprawdzenie czy da się wygenerować permutację z listy generatorów *)
  val is_generated : key -> key list -> bool
end

module Make(Perm: PermType) = struct
  type key = Perm.t
  module PermSet = Set.Make(Perm)
  type t = PermSet.t 
  let is_generated perm permList = 
    let rec generate_and_check perm permSet = 
      if PermSet.mem perm permSet 
      then true
      else let nextPermSet = 
             PermSet.fold 
             (fun perm1 pSet1 -> PermSet.fold (fun perm2 pSet2 -> (PermSet.add (Perm.compose perm1 perm2) pSet2)) permSet pSet1)
             permSet
             (PermSet.fold (fun perm pSet -> PermSet.add (Perm.invert perm) pSet) permSet permSet) 
           in if (PermSet.compare permSet nextPermSet) = 0 
              then false
              else generate_and_check perm nextPermSet
    in generate_and_check perm (List.fold_left (fun xSet x -> PermSet.add x xSet) PermSet.empty permList)
end

