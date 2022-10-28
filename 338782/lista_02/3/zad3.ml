let rec tails = function 
    | [] -> [[]]
    | (_::xs) as x-> x :: tails xs

let rec inits ?(acc=[]) xs = match xs with
    | [] -> [List.rev acc]
    | x::xs -> List.rev acc :: inits xs ~acc:(x::acc)

