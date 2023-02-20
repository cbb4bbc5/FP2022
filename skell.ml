(* lexer *)

type token = 
  | EOL | IgnoreEOL     (* \n  ^ *)
  | Input | Output      (* @  @<< *)
  | Begin of string     (* [  (  -> *)
  | End of string       (* ]  )  ; *)
  | Asign | Next        (* :=  , *)
  | If | Elif | Else    (* ?  |  ! *)
  | Function | Return   (* \  \<< *)
  | While | Continue    (* $  $<< *)
  | Boolean of bool     (* #t  #f *)
  | Operator of string  (* +  -  *  /  %  <  <=  =  >=  >  <>  &&  || *)
  | Numeric of string   (* 1  -1  1/2  -1/2 *)
  | Text of string      (* "abc" *)
  | Symbol of string    (* _aA1 *)
  | ListBegin | BlockBegin | Other of string

let matches regstr text =
  Str.string_match (Str.regexp regstr) text 0

let trim_sides str =
  String.sub str 1 (String.length str - 2)

let regbegin = {|\(begin\|\[\|{\|(\|->\)$|}
let regend = {|\(end\|\]\|\}\|)\|;\)$|}
let regin = {|\(input\|@\)$|}
let regout = {|\(output\|@<<\)$|}
let regif = {|\(if\|\?\)$|}
let regwhile = {|\(while\|\$\)$|}
let regfun = {|\(fun\|λ\|\\\)$|}
let regtrue = {|\(true\|#t\)$|}
let regfalse = {|\(false\|#f\)$|}
let regret = {|\(return\|λ<<\|\\<<\)$|}
let regelif = {|\(elif\||\)$|}
let regelse = {|\(else\|!\)$|}
let regcont = {|\(continue\|\$<<\)$|}
(* let regbreak = {|\(break\|\$!!\)$|} *)
let regop = {|\(\+\|-\|\*\|/\|%\|<\|<=\|=\|>=\|>\|<>\|||\|&&\)$|}
let regnum = {|-?[0-9].*|}
let regstr = "'.*'$"

let lex_token token =
  if token = "\n" then EOL
  else if token = "^" then IgnoreEOL
  else if matches regin token then Input
  else if matches regout token then Output
  else if token = ":=" then Asign
  else if matches regbegin token then Begin token
  else if matches regend token then End token
  else if token = "," then Next
  else if matches regif token then If
  else if matches regelif token then Elif
  else if matches regelse token then Else
  else if matches regfun token then Function
  else if matches regret token then Return
  else if matches regwhile token then While
  else if matches regcont token then Continue
  else if matches regtrue token then Boolean true
  else if matches regfalse token then Boolean false
  else if matches regop token then Operator token
  else if matches regnum token then Numeric token
  else if matches regstr token then Text (trim_sides token)
  else Symbol token

let char_to_str = String.make 1
let char_eq c = (=) c

let lex text = 
  let rec apply_spaces str k =
    try let c = str.[k]
      in (if char_eq c '\r' then ""
      else if char_eq c '[' then "[ "
      else if char_eq c ']' then " ]"
      else if List.exists (char_eq c) 
        [','; ';'; '('; ')'; '\n'] then " " ^ char_to_str c ^ " "
      else char_to_str c) ^ apply_spaces str (k + 1)
    with Invalid_argument _ -> " \n"
  in let tokens = Str.split (Str.regexp " +") (apply_spaces text 0)
  in List.map lex_token tokens

(* nester *)

type nested = 
  | Resume
  | Expr of token list
  | Block of nested list

let counterpart_of = function
  | Begin "->" -> End ";"
  | Begin "(" -> End ")"
  | Begin "{" -> End "}"
  | Begin "[" -> End "]"
  | _ -> raise (Failure "Unknown opening token")
(*| Begin "{" -> End "}"*)

let rec split_on_eol expr tokens = match tokens with
  | Begin b as esc :: rest -> if b = "["
    then Expr (List.rev (ListBegin :: expr)), esc, false, rest
    else Expr (List.rev (BlockBegin :: expr)), esc, false, rest
  (* | End _ as esc :: EOL :: IgnoreEOL :: rest -> Expr (List.rev expr), esc, true, rest *)
  (* | End _ as esc :: EOL :: rest -> Expr (List.rev expr), esc, false, rest *)
  | End _ as esc :: rest -> Expr (List.rev expr), esc, false, rest (* true *)
  | EOL :: IgnoreEOL :: rest -> split_on_eol expr rest
  | EOL :: rest -> begin match expr with
    | escape_token :: _ -> Expr (List.rev expr), escape_token, false, rest
    | [] -> Expr [], EOL, false, rest end
  | token :: rest -> split_on_eol (token :: expr) rest
  | [] -> begin match expr with
    | escape_token :: _ -> Expr (List.rev expr), escape_token, false, []
    | [] -> Expr [], End "EOF", false, [] end

let update exprs expr resume = 
  match expr, resume with
  (* | Expr [], true -> Resume :: exprs *)
  | Expr [], false -> exprs
  (* | _ as expr, true -> Resume :: expr :: exprs *)
  | _ as expr, false -> expr :: exprs

let rec nest_token_expressions tokens = function End end_sign ->
  let rec nest tokens = begin function Block exprs -> 
    let expr, line_escape_token, resume, rest = split_on_eol [] tokens
    in begin match line_escape_token with
    | Begin a as beg -> 
      let block, rest = nest_token_expressions rest (counterpart_of beg)
      in nest rest (Block (block :: update exprs expr resume))
    | End sign ->
      if sign = end_sign then Block (List.rev (update exprs expr resume)), rest
      else raise (Failure ("Unmatched escape token: " ^ sign ^ " =/= " ^ end_sign))
    | _ -> nest rest (Block (update exprs expr resume)) end
  | _ -> raise (Failure "Nesting function require Block constructor") end
  in nest tokens (Block [])
| _ -> raise (Failure "Illegal escape token")

let distinct_blocks tokens = fst (nest_token_expressions tokens (End "EOF"))

(* parser *)

type symbol = string

type expr =
  | UnitE
  | VarE of symbol
  | AsgE of symbol * expr
  | OpE of symbol * expr * expr
  | AppE of expr * expr
  | FunE of symbol * expr
  | BitE of bool
  | NumE of int * int
  | StrE of string
  | LstE of expr list
  | WhileE of expr * expr
  | IfE of expr * expr * expr
  | InputE
  | OutputE of expr
  | OrderE of expr * expr
  | BlockE of expr list
  | ReturnE
  | EmptyE
  | TestE of token list

(* type token = 
  | EOL | IgnoreEOL     (* \n  ^ *)
  | Input | Output      (* @  @<< *)
  | Begin of string     (* [  (  -> *)
  | End of string       (* ]  )  ; *)
  | Asign | Next        (* :=  , *)
  | If | Elif | Else    (* ?  |  ! *)
  | Function | Return   (* \  \<< *)
  | While | Continue    (* $  $<< *)
  | Boolean of bool     (* #t  #f *)
  | Operator of string  (* +  -  *  /  %  <  <=  =  >=  >  <>  &  | *)
  | Numeric of string   (* 1  -1  1/2  -1/2 *)
  | Text of string      (* "abc" *)
  | Symbol of string    (* _aA1 *)
  | ListBegin | Other of string *)

let matches_int num =
  matches {|-?\(0\|[1-9][0-9]*\)$|} num

let convert_to_num num =
  match String.split_on_char '/' num with
  | a :: b :: [] when matches_int a && matches_int b -> 
    NumE (int_of_string a, int_of_string b)
  | n :: [] when matches_int n -> NumE (int_of_string n, 1)
  | _ -> raise (Failure ("Invalid number: " ^ num))

let rec parse_token = function
| Symbol x -> VarE x
| Numeric num -> convert_to_num num
| Input -> InputE
| Boolean b -> BitE b
| _ -> raise (Failure "Tokenparsing nontoken") 

let init_result x = x
let init_prior = EmptyE

let rec parse expr prior result = match expr with
  | Block [] -> EmptyE, fun x -> result x
  | Block (exprs) -> 
    let internal_type = ref init_prior
    in let internal_result = ref init_result
    in let block = ref []
    in begin 
      List.iter (fun expr -> 
        let next, new_result = parse expr !internal_type !internal_result
        in begin
          if !internal_type = EmptyE
          then block := (fun x -> new_result x) :: !block
          else block := (fun x -> !internal_result (new_result x)) :: (List.tl !block);
          internal_type := next;
          internal_result := new_result;
        end) exprs;
      prior, fun x -> BlockE (List.map (fun x -> x EmptyE) !block) end
  | Expr (tokens) when prior = EmptyE -> begin match tokens with 
    | Symbol s :: Asign :: rest ->
      let _, cont = parse (Expr rest) init_prior init_result
      in EmptyE, fun x -> AsgE (s, cont EmptyE)
    | (Symbol _ as a) :: Symbol _ :: rest 
    | (Symbol _ as a) :: Numeric _ :: rest 
    | (Symbol _ as a) :: Boolean _ :: rest 
    | (Symbol _ as a) :: Input _ :: rest ->
      let _, cont = parse (Expr (List.tl tokens)) (AppE (EmptyE, EmptyE)) (fun x -> 
        AppE (parse_token a, x))
      in EmptyE, fun x -> cont x
    | (Symbol _ as a) :: Operator op :: rest
    | (Numeric _ as a) :: Operator op :: rest
    | (Boolean _ as a) :: Operator op :: rest
    | (Input _ as a) :: Operator op :: rest ->
      let _, cont = parse (Expr rest) init_prior init_result
      in EmptyE, fun x -> OpE (op, parse_token a, cont EmptyE)
    | (Symbol _ as a) :: rest
    | (Numeric _ as a) :: rest
    | (Boolean _ as a) :: rest
    | (Input _ as a) :: rest ->
      let _, cont = parse (Expr rest) init_prior init_result
      in EmptyE, fun x -> parse_token a
    | Output :: rest -> 
      let _, cont = parse (Expr rest) init_prior init_result
      in EmptyE, fun x -> OutputE (cont EmptyE)
    | _ -> EmptyE, result end
  | Expr (tokens) when prior = AppE (EmptyE, EmptyE) -> begin match tokens with
    | (_ as a) :: Symbol _ :: rest
    | (_ as a) :: Numeric _ :: rest
    | (_ as a) :: Boolean _ :: rest
    | (_ as a) :: Input _ :: rest -> 
      let _, cont = parse (Expr (List.tl tokens)) prior (fun x -> 
        (AppE (result (parse_token a), x)))
      in EmptyE, fun x -> cont x
    | (Symbol _ as a) :: rest
    | (Numeric _ as a) :: rest
    | (Boolean _ as a) :: rest
    | (Input _ as a) :: rest -> EmptyE, (fun x -> result (parse_token a))
    | [] -> EmptyE, result end
  | Resume -> prior, result


let handle text = 
  (snd (parse (distinct_blocks (lex text)) init_prior init_result)) EmptyE

(* fib |> lex |> distinct_blocks *)

let num = "
0/2
"
let noblock = "
a := 2
b := c + 2
c := 42 * f
f a b c
@<< a + b
f @ 42
a && #t || b
"

let fib = "
  fib := \ n ->
    f := [0, 1]
    while n > 1 ->
      f := [f.1 , f.0 + f.1]
      n := n - 1 ;
    f.n ;
  fib 5
"
let fold = "
  (fold (λ a 
  ^ b -> while ->
    a + b;
    ;)
   + a
  ^ 7)
"
