module type OrderedType = sig
  type t
  val compare : t -> t -> int
end


module Make = functor (Key : OrderedType) -> struct
  type key = Key.t

  module M = Map.Make(Key)

  let apply m k = 
    let x,y = m in
      let present = M.find_opt k x
        in match present with 
        |Some value -> value
        |None -> k

  let id = M.empty, M.empty
  let invert m = let x,y = m in y,x

  let swap k1 k2 =
    if (k1 != k2)
    then
      let swapperm = 
        M.add k2 k1 (M.add k1 k2 M.empty)
      in swapperm,swapperm
    else id

  

  let compare m1 m2 = 
    M.compare Key.compare m1 m2


  (* compose m m = m(n(k)) *)
  let compose m n =

    let merger k mk nk =
      (match mk,nk with
      | vm, None -> vm 
      | _, Some nk ->
        (let finval = apply m nk
        in if finval != k
           then Some finval
           else None))

    in let xm,ym = m in
      let xn,yn = n in
        (M.merge merger xm xn), (M.merge merger yn ym)
      
    

  

end