module STB(State : sig type t end) :
sig 
    type 'a t
    val return : 'a -> 'a t
    val bind : 'a t -> ('a -> 'b t) -> 'b t
    val fail : 'a t
    val flip : bool t
    val get  : State.t t
    val put  : State.t -> unit t
    val run  : State.t -> 'a t -> 'a Seq.t
end =
struct
    type 'a t = State.t -> ('a * State.t) Seq.t
    let return x s = Seq.cons (x,s) Seq.empty
    let bind m f s = Seq.concat_map (fun (a,s) -> f a s) (m s) 
    let fail s = Seq.empty
    let flip s = List.to_seq [(true,s);(false,s)]
    let get  s = Seq.cons (s,s) Seq.empty
    let put  s s1= Seq.cons ((),s) Seq.empty
    let run s m = Seq.map fst (m s)
end

