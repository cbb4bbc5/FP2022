type 'a dllist = 'a dllist_data lazy_t
and 'a dllist_data =
    { prev : 'a dllist
    ; elem : 'a
    ; next : 'a dllist
    }

let prev dl = (Lazy.force dl).prev
let next dl = (Lazy.force dl).next
let elem dl = (Lazy.force dl).elem
let rec make_dllist ?(pre=None) ?(nex=None) (scb,scf,gelem as fs) xs =
    let rec this = lazy
        {prev=
            begin match pre with
            | Some x -> x
            | None -> make_dllist fs (scb xs) ~nex:(Some this)
            end
        ; elem= gelem xs
        ; next=
            begin match nex with
            | Some x -> x
            | None -> make_dllist fs (scf xs) ~pre:(Some this)
            end
        }
    in this



let of_list xs =
    let rec scroll_back = function 
        | (_,[],rzs,zs) | ([],_,rzs,zs) -> scroll_back (rzs,zs,rzs,zs)
        | (x::rxs,xs,rzs,zs) -> (rxs,x::xs,rzs,zs)
    in
    let rec scroll_forw = function 
        | (_,[],rzs,zs) | ([],_,rzs,zs) -> scroll_forw (rzs,zs,rzs,zs)
        | (rxs,x::xs,rzs,zs) -> (x::rxs,xs,rzs,zs)
    in
    let rec get_elem = function
        | (_,x::xs,_,_) -> x
        | (_,_,_,zs) -> get_elem (zs,zs,zs,zs)
    in
    
    match xs with
    | [] -> failwith "Empty list"
    | xs -> let rxs = List.rev xs in 
        make_dllist (scroll_back,scroll_forw,get_elem) (rxs,xs,rxs,xs)




let integers = make_dllist (Fun.flip (-) 1,(+) 1,Fun.id) 0 

