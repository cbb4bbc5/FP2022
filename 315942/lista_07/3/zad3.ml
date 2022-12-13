let return_id x = x
let rec bind_id m f = f m
let (let* ) = bind_id

let return_cont x = fun () -> x
let rec bind_cont m f = f (m ())
let (let* ) = bind_cont;;