open Zad4

let ints_node f i = lazy {
  prev = f (i - 1);
  elem = i;
  next = f (i + 1)
}

let integers = Zad1.fix_memo ints_node 0
