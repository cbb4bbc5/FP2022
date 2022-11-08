type 'a heap
val size : 'a heap -> int
val empty : 'a heap
val push : 'a -> 'a heap -> 'a heap
val pop : 'a heap -> ('a option * 'a heap)

