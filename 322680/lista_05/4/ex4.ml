type 'a dllist = 'a dllist_data lazy_t
and 'a dllist_data = {
  prev : 'a dllist;
  elem : 'a;
  next : 'a dllist;
}

let prev ((lazy (dll : 'a dllist_data)) : 'a dllist) = dll.prev

let elem ((lazy (dll : 'a dllist_data)) : 'a dllist) = dll.elem

let next ((lazy (dll : 'a dllist_data)) : 'a dllist) = dll.next

let of_list =
  let rec get_next xs prev head =
    match xs with
    | [] -> let (lazy head) = head in head
    | x :: xs ->
      let rec curr = {
        prev = prev;
        elem = x;
        next = lazy (get_next xs (lazy curr) head);
      } in curr

  in let get_last curr head =
       let (lazy curr) = curr
    in let (lazy head) = head
    in let rec eager_traverse curr head =
      let (lazy next) = curr.next in
      if next == head then curr
      else eager_traverse next head
    in eager_traverse curr head

  in function
  | [] -> failwith "of_list []"
  | x :: xs ->
    let rec head = lazy {
      prev = lazy (get_last head head);
      elem = x;
      next = lazy (get_next xs head head);
    } in head
