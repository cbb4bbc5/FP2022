type 'a list2 = private
| Nil
| Zero of ('a * 'a) list2
| One of 'a * ('a * 'a) list2
| Two of ('a * 'a) * ('a * 'a) list2

val empty : 'a list2

val cons : 'a. 'a -> 'a list2 -> 'a list2

val hd : 'a. 'a list2 -> 'a option

val tl : 'a. 'a list2 -> 'a list2 option

val nth : 'a. int -> 'a list2 -> 'a option
