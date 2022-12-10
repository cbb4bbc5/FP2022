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
    type 'a t = State.t -> 'a sbt_list
    and 'a sbt_list = 
    | Nil
    | Cons of 'a * State.t * 'a t
    let rec append m1 m2 s = 
        match m1 s with
        | Nil  -> m2 s
        | Cons(a,s1,m12) -> Cons(a,s1,append m12 m2)
    let fail s = Nil
    let return a s = Cons(a,s,fail)
    let rec bind m f s = 
        match m s with
        | Nil -> Nil
        | Cons(a,s1,m1) -> append (f a) (bind m1 f) s1
    let flip s = Cons(true,s,fun s1 -> Cons(false,s1,fail))
    let get s = Cons(s,s,fail)
    let put new_s s = Cons((),new_s,fail) 
    let rec run s m = 
        match m s with
        | Nil -> fun () -> Seq.Nil
        | Cons(a,s1,m1) -> fun () -> Seq.Cons(a,run s1 m1)
end
