type 'a dllist = 'a dllist_data lazy_t
and 'a dllist_data = {
  prev : 'a dllist;
  elem : 'a;
  next : 'a dllist;
}

let prev ((lazy (dll : 'a dllist_data)) : 'a dllist) = dll.prev

let elem ((lazy (dll : 'a dllist_data)) : 'a dllist) = dll.elem

let next ((lazy (dll : 'a dllist_data)) : 'a dllist) = dll.next

let rec next_int n prev =
  let rec curr = lazy {
    prev = prev;
    elem = n;
    next = next_int (n + 1) curr
  } in curr

let rec prev_int n next =
  let rec curr = lazy {
    prev = prev_int (n - 1) curr;
    elem = n;
    next = next
  } in curr

let integers =
  let rec zero = lazy {
    prev = prev_int (-1) zero;
    elem = 0;
    next = next_int 1 zero;
  } in zero
