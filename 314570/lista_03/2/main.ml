module IntPerm = Perm.Make(Int) 
module IntPermGenerators = Generators.Make(IntPerm) 
let perm1 = IntPerm.swap 1 3
let perm2 = IntPerm.swap 2 1
let perm3 = IntPerm.swap 4 5
let perm4 = IntPerm.swap 3 5
let perm5 = IntPerm.swap 5 1
let perm6 = IntPerm.swap 10 11


let arr = [perm1;perm2;perm3;perm4;perm5] 

let _ = if (IntPermGenerators.is_generated perm1 arr) = true then print_endline "1. passed" else print_endline "1. failed"
let _ = if (IntPermGenerators.is_generated (IntPerm.compose perm2 perm1) arr) = true then print_endline "2. passed" else print_endline "2. failed"
let _ = if (IntPermGenerators.is_generated (IntPerm.invert perm1) arr) = true then print_endline "3. passed" else print_endline "3. failed"
let _ = if (IntPermGenerators.is_generated perm6 arr) = false then print_endline "4. passed" else print_endline "4. failed"

let comples_perm = IntPerm.compose (IntPerm.compose (IntPerm.invert perm3) perm2) (IntPerm.invert (IntPerm.compose perm1 (IntPerm.compose perm4 perm5)))

let _ = if (IntPermGenerators.is_generated comples_perm arr) = true then print_endline "5. passed" else print_endline "5. failed"

