module type OrderedPermType = Perm.S

module type S = sig
  type perm
  val is_generated : perm -> perm list -> bool
end

module Make(Perm : OrderedPermType) : S with type perm = Perm.t
