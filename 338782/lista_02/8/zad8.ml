
type 'a heap = 
    | Leaf
    | Node of 'a heap * int * 'a * 'a heap

let size = function
    | Leaf -> 0
    | Node(l,s,n,r) -> s

let empty = Leaf

let add_branches x l r = if size l > size r 
    then Node(l,size r+1,x,r)
    else Node(r,size l+1,x,l)

let rec union x y = match x,y with
    | Leaf,x | x,Leaf -> x
    | (Node(xl,xs,xx,xr) as xn),(Node(yl,ys,yy,yr) as yn) -> if xx<yy
        then add_branches xx xl (union xr yn)
        else add_branches yy yl (union yr xn)

let push x tree = union (Node(Leaf,1,x,Leaf)) tree
let pop = function 
    | Leaf -> (None,Leaf)
    | Node(l,s,x,r) -> (Some x,union l r)

