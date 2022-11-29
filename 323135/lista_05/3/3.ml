type tree = Node of (int * int) * (unit -> tree) * (unit -> tree)

let rec create (a, b) (c, d) () =
  let nval = (a + c, b + d)
  in Node(nval,
          create (a, b) nval,
          create nval (c, d))

let get_left t = match t with
    | Node(_, l, _) -> l ()

let rec go_left n t  = 
  if n = 0
    then t
  else
    go_left (n - 1) (get_left t)

let get_right t = match t with
    | Node(_, _, r) -> r ()

let rec go_right n t = 
  if n = 0
    then t
  else
    go_left (n - 1) (get_right t)

let q_set = create (0, 1) (1, 0)

q_set () |> go_right 1 |> go_right 1 |> go_right 1 |> go_right 1;;
