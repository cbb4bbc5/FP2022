type 'a clist = { clist : 'z. ('a -> 'z -> 'z) -> 'z -> 'z }
let cnil : 'a clist = { clist = fun f z -> z }
let ccons a cls : 'a clist = { clist = fun f z -> f a (cls.clist f z) }
let map g cls : 'b clist = { clist = fun f z -> cls.clist (fun x y -> f (g x) y) z }
let append cl1 cl2 : 'a clist = { clist = fun f z -> cl1.clist f (cl2.clist f z) }
let clist_to_list cl = cl.clist (fun x y -> x :: y) []
let rec list_to_clist ls : 'a clist = { clist = fun f z -> if ls = [] then z else f (List.hd ls) ((list_to_clist (List.tl ls)).clist f z) }

let prod (cl1 : 'a clist) (cl2 : 'b clist) : ('a * 'b) clist = { clist = fun f z -> cl1.clist (fun x y -> cl2.clist (fun a b -> f (x, a) b) y) z }

let list1 = ccons 2 (ccons 1 (ccons 3 (ccons 7 cnil)))
let list2 = ccons 1 (ccons 2 (ccons 3 cnil))
