type 'a zlist = {
  pref: 'a list;
  suf:  'a list
}

let of_list xs = {
  pref = [];
  suf = xs
}

let elem zxs =
  let {pref;suf} = zxs in
  match suf with
  | [] -> None
  | x :: _ -> Some(x)

let to_list zxs = 
  let {pref;suf} = zxs in
  List.rev_append pref suf

let move_left zxs =
  let {pref;suf} = zxs in
  match pref with
  | [] -> {pref;suf}
  | x :: new_pref -> {pref = new_pref; suf = x :: suf}

let move_right zxs =
  let {pref;suf} = zxs in
  match suf with
  | [] -> {pref;suf}
  | x :: new_suf -> {pref = x :: pref; suf = new_suf}

let insert x zxs=
  let { pref; suf} = zxs in
  { pref = x :: pref; suf= suf}

let remove zxs =
  let {pref; suf} = zxs in
  match pref with
  | []          -> { pref; suf }
  | _ :: pref -> { pref; suf }
