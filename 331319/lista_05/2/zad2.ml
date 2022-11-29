type 'a loop = In of ('a loop -> 'a)
let out x = match x with In f -> f 

let fix = fun f -> (fun z a -> f ((out z) z) a) 
               (In (fun z a -> f ((out z) z) a))

let fix_state =
  fun g ->
    (fun f -> 
      f := g (fun x -> !f x) ; f 
    ) (ref (fun x -> 2137))
;;

(* Przykład: samoaplikacja w OCamlu.
   Co zrobić, żeby otypowała się silnia z wykładu z metod? 
   fact_f: (int -> int) loop -> int -> int
        f: (int -> int) loop
  (out f): (int -> int) loop -> int -> int
  Więc teraz można przekazać f do (out f). A następnie inta i spodziewać się inta.
*)
let fact n = 
  let fact_f f n = 
    if n <= 1 then 1
    else n * (((out f) f) (n - 1))
  in fact_f (In fact_f) n
;;
