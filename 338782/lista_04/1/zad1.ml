type 'a blist =
  | Nil
  | Zero of ('a * 'a) blist
  | One of 'a * ('a * 'a) blist
  | Two of 'a * 'a * ('a * 'a) blist

let rec bcons : 'a. 'a -> 'a blist -> 'a blist = fun x xs -> 
  match xs with
  | Nil -> One(x,Nil)
  | Zero xs -> One(x,xs)
  | One(y,xs) -> Two(x,y,xs)
  | Two(y,z,xs) -> One(x,bcons (y,z) xs)

let rec bnth : 'a. int -> 'a blist -> 'a = fun n xs ->
  match xs with
  | Nil -> failwith "Out of bounds"
  | Zero xs -> 
      let (x,y) = bnth (n/2) xs in
        if n mod 2 == 0
          then x
          else y
  | One (x,xs) -> 
      begin match n with
        | 0 -> x
        | _ -> bnth (n-1) (Zero xs)
      end
  | Two (x,y,xs) -> 
      begin match n with
        | 0 -> x
        | 1 -> y
        | _ -> bnth (n-2) (Zero xs)
      end

let rec bpop : 'a. 'a blist -> 'a * 'a blist =
  function
    | Nil -> failwith "Out of bounds"
    | Zero xs -> let ((x,y),xs) = bpop xs in x,One(y,xs)
    | One(x,xs) -> (x,Zero xs)
    | Two(x,y,xs) -> (x,One(y,xs))

let btl xs = let (_,xs) = bpop xs in xs
let bhd xs = bnth 0 xs

let rec bof_list = function
    | [] -> Nil
    | x::xs -> bcons x (bof_list xs)
