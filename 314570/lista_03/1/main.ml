module StringPerm = Perm.Make(String)

let perm1 = StringPerm.swap "a" "b"
let perm2 = StringPerm.swap "c" "b"
let perm3 = StringPerm.swap "b" "c"

let perm13 = StringPerm.compose perm1 perm3 

let _ = if (StringPerm.apply perm1 "a") = "b" then print_endline "1. passed" else print_endline "1. failed"
let _ = if (StringPerm.apply perm13 "a") = "c" then print_endline "2. passed" else print_endline "2. failed"
let _ = if (StringPerm.apply (StringPerm.invert perm13) "b") = "c" then print_endline "3. passed" else print_endline "3. failed"
let _ = if (StringPerm.compare perm1 perm2) = 0 then print_endline "4. failed" else print_endline "4. passed"
let _ = if (StringPerm.compare StringPerm.id StringPerm.id) = 0 then print_endline "5. passed" else print_endline "5. failed"
let _ = if (StringPerm.compare (StringPerm.compose perm13 (StringPerm.invert perm13)) StringPerm.id) = 0 then print_endline "6. passed" else print_endline "6. failed"

