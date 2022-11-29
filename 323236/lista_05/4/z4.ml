type 'a dllist = 'a dllist_data lazy_t
and 'a dllist_data =
{ prev : 'a dllist;
 elem : 'a;
 next : 'a dllist;
}

exception Dllist_exception of string;;


let next x = (Lazy.force x).next
let prev x = (Lazy.force x).prev
let elem x = (Lazy.force x).elem


let of_list xs =
  let n = List.length xs
    in let my_list = Array.make n (lazy (raise (Dllist_exception "unused"))) in
        List.iteri (fun i x ->
          Array.set my_list i (lazy {
            prev = my_list.((n + i - 1) mod n);
            next = my_list.((i + 1) mod n); 
            elem = x;
          }))
          xs;
  my_list.(0)


