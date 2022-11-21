type 'a list2 =
| Nil
| Zero of             ('a * 'a) list2
| One  of        'a * ('a * 'a) list2
| Two  of ('a * 'a) * ('a * 'a) list2

let empty = Nil

(* TODO: pretty-printer list2 *)

let rec cons : 'a. 'a -> 'a list2 -> 'a list2 =
  fun x xs ->
  match xs with
  | Nil             -> One(x, Nil)
  | Zero(xs)        -> One(x, xs)
  | One(y, xs)      -> Two((x, y), xs)
  | Two((y, z), xs) -> One(x, cons (y, z) xs)

let rec hdtl : 'a. 'a list2 -> ('a * 'a list2) option =
  function
  | Nil             -> None
  | Zero(xs) ->
    begin match hdtl xs with
    | None          -> None
    | Some ((x, y), xs)
                    -> Some (x, One(y, xs))
    end
  | One(x, xs)      -> Some (x, Zero(xs))
  | Two((x, y), xs) -> Some (x, One(y, xs))

let hd : 'a. 'a list2 -> 'a option =
  fun xs ->
  match hdtl xs with
  | None -> None
  | Some (hd, tl) -> Some hd

let tl : 'a. 'a list2 -> 'a list2 option =
  fun xs ->
  match hdtl xs with
  | None -> None
  | Some (hd, tl) -> Some tl

let rec nth : 'a. int -> 'a list2 -> 'a option =
  fun n xs ->
  match xs with
  | Nil           -> None
  | Zero(xs) ->
    begin match nth (n / 2) xs with
    | None        -> None
    | Some (x, y) ->
      if n mod 2 = 0
      then           Some x
      else           Some y
    end
  | One(x, _) when n = 0
                  -> Some x
  | One(_, xs)    -> nth (n - 1) (Zero(xs))
  | Two((x, y), xs) ->
    begin match n with
    | 0           -> Some x
    | 1           -> Some y
    | n           -> nth (n - 2) (Zero(xs))
    end