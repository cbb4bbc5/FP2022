type lrt = {
  l : (unit -> lrt);
  mid : (int * int);
  r : (unit -> lrt);
}


let rec rat a b c d = {
  mid = (a+b,b+c);
  l = (fun () -> rat a b (a+c) (b+d));
  r = (fun () -> rat (a+c) (b+d) c d);
}

let mid x = x.mid;;

let left x = x.l ();;

let right x = x.r ();;

let rat_pos = rat 0 1 1 0;;