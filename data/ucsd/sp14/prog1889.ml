
let rec wwhile (f,b) =
  match f b with | (b',c') -> if c' then wwhile (f, b') else b';;

let fixpoint (f,b) = ((wwhile (fun x  -> ((not x), ((f b) = b)))), b);;