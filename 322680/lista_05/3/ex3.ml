type ltree = Node of thonk * (int * int) * thonk
and thonk = unit -> ltree

let rec rationals (a, b) (c, d) =
  fun () -> let root = (a + c, b + d)
  in Node(rationals (a, b) root, root, rationals root (c, d))

let q = rationals (0, 1) (1, 0)
