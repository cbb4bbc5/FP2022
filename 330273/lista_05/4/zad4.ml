type 'a dllist = 'a dllist_data lazy_t
and 'a dllist_data =
{ prev : 'a dllist
; elem : 'a
; next : 'a dllist
}

let prev dl =
    let dt  = Lazy.force dl in
    dt.prev

let elem dl =
    let dt  = Lazy.force dl in
    dt.elem

let next dl =
    let dt  = Lazy.force dl in
    dt.next

let rec of_list xs =
    let rec sing x = lazy { prev = (sing x); elem = x; next = (sing x)  } in
    let rec gen prev xs first =
        match xs with
        | []    -> first, prev
        | x::xs ->
                let last = ref (sing x) in
                let rec z = lazy begin
                    let (a,b)  = gen z xs first in
                    last := b; { prev = prev; elem = x; next = a}
                end in
                let k = (Lazy.force z) in 
                lazy k, !last in
    match xs with
    | [] -> failwith "err"
    | x::xs ->
            let rec first = lazy begin
                let (a,b) = gen first xs first in
                { prev = b; elem = x; next = a }
            end in first

let integers =
    let rec aux n =
        lazy { prev = aux (n-1) ; elem = n ; next = aux (n+1) } in
    aux 0
