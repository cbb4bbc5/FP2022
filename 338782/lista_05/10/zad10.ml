let hd sq = match sq() with
| Seq.Nil -> failwith "Empty"
| Seq.Cons(x,_) -> x
let tl sq = match sq() with
| Seq.Nil -> failwith "Empty"
| Seq.Cons(_,x) -> x
let rec to_list xs = match xs() with
| Seq.Nil -> []
| Seq.Cons(x,xs) -> x::to_list xs

type empty = |

type _ fin_type =
| Unit : unit fin_type
| Bool : bool fin_type
| Pair : 'a fin_type * 'b fin_type -> ('a * 'b) fin_type
| Either : 'a fin_type * 'b fin_type -> ('a , 'b) Either.t fin_type
| Empty : empty fin_type
| Function : ('a fin_type * 'b fin_type) -> ('a -> 'b) fin_type

let rec zip base_a base_b ?(a=base_a) ?(b=base_b) = match (a(),b()) with
    | (Seq.Nil,Seq.Nil) -> fun () -> Seq.Nil
    | (Seq.Nil,_) -> zip base_a base_b ~a:base_a ~b:b
    | (_,Seq.Nil) -> zip base_a base_b ~a:a ~b:base_b
    | (Seq.Cons(x,xs),Seq.Cons(y,ys)) -> fun () -> Seq.Cons((x,y),zip base_a base_b ~a:xs ~b:ys)

let rec prod a b = Seq.flat_map (fun x -> Seq.map (fun elt () -> Seq.Cons(elt,x)) b) a
let rec all_values : type c. c fin_type -> c Seq.t = function 
    | Unit -> fun () -> Seq.Cons((),fun () -> Seq.Nil)
    | Bool -> fun () -> Seq.Cons(true,fun () -> Seq.Cons(false,fun () -> Seq.Nil))
    | Pair(a,b) -> let ys = all_values a in Seq.flat_map (fun x -> Seq.map (fun elt -> (elt,x)) ys) (all_values b)
    | Either(a,b) -> Seq.append (Seq.map (fun x -> Either.Left x) (all_values a)) (Seq.map (fun x -> Either.Right x) (all_values b))
    | Function(a,b) -> 
        let ab=all_values b and aa=all_values a
        in 
        let vals = Seq.fold_left (fun x y -> prod x ab) 
                (Seq.map (fun x () -> Seq.Cons(x,fun () -> Seq.Nil)) ab) aa
        in 
            Seq.map (to_func a aa) vals
    | Empty -> fun () -> Seq.Nil
and fin_equal : type ac. ac fin_type -> ac -> ac -> bool = fun fin a b -> match fin with
    | Unit -> true
    | Empty -> true
    | Bool -> a=b
    | Pair(f,g) -> fin_equal f (fst a) (fst b) && fin_equal g (snd a) (snd b)
    | Either(f,g) -> begin match (a,b) with
        | (Left _,Right _) | (Right _,Left _) -> false
        | (Left a,Left b) -> fin_equal f a b
        | (Right a,Right b) -> fin_equal g a b
        end
    | Function(f,g) -> true
and find : type f g. f fin_type -> f -> (f * g) Seq.t -> g = 
    fun fin x xs -> match xs() with
        | Seq.Nil -> raise Not_found
        | Seq.Cons((x1,y),xs) when fin_equal fin x1 x -> y
        | Seq.Cons(_,xs) -> find fin x xs
and to_func : type a b. a fin_type -> a Seq.t -> b Seq.t -> a -> b = fun a aa xs ->
    let zipped : (a*b) Seq.t = zip aa xs in
        fun x -> find a x zipped


