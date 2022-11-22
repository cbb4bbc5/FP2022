type 'a zlist

val of_list : 'a list -> 'a zlist

val to_list : 'a zlist -> 'a list

val elem : 'a zlist -> 'a option

val move_left : 'a zlist -> 'a zlist

val move_right : 'a zlist -> 'a zlist

val insert : 'a -> 'a zlist -> 'a zlist

val remove : 'a zlist -> 'a zlist
