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

module Make(Perm: PermType) : S with type key = Perm.t
