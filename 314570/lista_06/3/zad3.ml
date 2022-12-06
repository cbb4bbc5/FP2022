let for_all f xs = 
  try List.fold_left 
      (fun _ y -> 
        match f y with
        | true -> true
        | false -> failwith "false")
      true
      xs
  with Failure _ -> false

let mult_list xs =
  try List.fold_left
      (fun acc x -> 
        match x with
        | 0 -> failwith "zero"
        | _ -> x * acc)
      1
      xs
  with Failure _ -> 0

let sorted xs =
  try (List.fold_left
      (fun last x ->
        match x > last with
        | true -> x 
        | false -> failwith "not sorted")
      (List.hd xs) 
      (List.tl xs)) |> Fun.const true
  with Failure _ -> false
