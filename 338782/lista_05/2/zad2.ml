type 'a rectype = {frec : 'a rectype -> 'a}
let fix1 : ((('a -> 'b) -> ('a -> 'b)) -> ('a -> 'b)) rectype -> (('a -> 'b) -> ('a -> 'b)) -> ('a -> 'b) = fun x f a -> f (x.frec x f) a
let fix_rectype f x = fix1 {frec = fix1} f x


let fix_while f x = 
    let tab = Hashtbl.create 10 in
    let stack = Stack.create() in
    let compute () = 
      Stack.push x stack;
        while not (Stack.is_empty stack) do
            let res = ref None in
            while Option.is_none !res do
            res := try Some (f (fun a -> match Hashtbl.find_opt tab a with
                | None -> (Stack.push a stack;failwith "") 
                | Some a -> a) (Stack.top stack))
                with _ -> None
            done;
            Hashtbl.replace tab (Stack.pop stack) (Option.get !res)
        done

in compute ();Hashtbl.find tab x


