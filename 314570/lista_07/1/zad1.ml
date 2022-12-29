module type RandomMonad = sig
    type 'a t
    val return : 'a -> 'a t
    val bind : 'a t -> ('a -> 'b t) -> 'b t
    val random : int t
end

module RS : sig 
    include RandomMonad
    val run     : int -> 'a t -> 'a
end 
=
struct
    type 'a t = int -> 'a * int

    let calc_rand seed = 16807 * (seed mod 127773) - 2836 * (seed / 127773)

    let run seed monad = fst (monad seed)

    let return num = fun seed -> (num, seed) 

    let bind m f seed = 
        let (rand, new_seed) = m seed in 
            f rand new_seed

    let random seed = 
        let rand = calc_rand seed in 
            let rand = if rand > 0 then rand else rand + 2147483647 in
                (rand, rand) 
end

module Shuffle(R: RandomMonad) : sig
    val shuffle : 'a list ->  'a list R.t
end
= 
struct
    let shuffle xs =
        let (let* ) = R.bind in
            let rec make_random_list xs acc =
                match xs with
                | [] -> R.return acc
                | (x::xs) ->
                    let* random = R.random in
                        make_random_list xs ((x,random)::acc)
            in 
                let* random_list = make_random_list xs [] in
                    R.return (random_list 
                              |> List.sort (fun (_, l_rand) (_, r_rand) -> r_rand - l_rand)
                              |> List.map fst)
end

let shuffle_monad =
  let ( let* ) = RS.bind in
  let module SF = Shuffle (RS) in
  let list = [1; 2; 3; 4; 5; 6; 7; 8] in
  let* shuffle_1 = SF.shuffle list in
  let* shuffle_2 = SF.shuffle list in
  let* shuffle_3 = SF.shuffle list in
  let* shuffle_4 = SF.shuffle list in
  let* shuffle_5 = SF.shuffle list in
  List.iter (Printf.printf "%d ") shuffle_1; print_newline ();
  List.iter (Printf.printf "%d ") shuffle_2; print_newline ();
  List.iter (Printf.printf "%d ") shuffle_3; print_newline ();
  List.iter (Printf.printf "%d ") shuffle_4; print_newline ();
  List.iter (Printf.printf "%d ") shuffle_5; print_newline ();
  RS.return ()

let _ = RS.run 42 shuffle_monad
