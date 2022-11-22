type 'a skew =
  | Nil
  | Zero of ('a * 'a) skew
  | One of 'a * ('a * 'a) skew
  | Two of 'a * 'a * ('a * 'a) skew


let rec cons : 'a. 'a -> 'a skew -> 'a skew = fun x xs -> 
  match xs with
  | Nil -> One(x, Nil)
  | Zero xs -> One(x, xs)
  | One(y, xs) -> Two(x, y, xs)
  | Two(y, z, xs) -> One(x, cons(y, z) xs)


let rec remove : 'a. 'a skew -> 'a * 'a skew =
  function
    | Nil -> failwith "Cannot remove"
    | Zero xs -> let ((x, y), xs) = remove xs in x, One(y, xs)
    | One(x, xs) -> (x,Zero xs)
    | Two(x, y, xs) -> (x, One(y, xs))


let rec nth : 'a. int -> 'a skew -> 'a = fun n xs ->
  match xs with
  | Nil -> raise Not_found
  | Zero xs -> 
      let (x, y) = nth (n / 2) xs in
        if n mod 2 == 0 then x else y
  | One (x, xs) -> 
      begin match n with
        | 0 -> x
        | _ -> nth (n - 1) (Zero xs)
      end
  | Two (x, y, xs) -> 
      begin match n with
        | 0 -> x
        | 1 -> y
        | _ -> nth (n - 2) (Zero xs)
      end
