
let pipe fs =
  let f a x result = a x in let base = f in List.fold_left f base fs;;

let _ = pipe [(fun x  -> x + 3); (fun x  -> x + x)] 3;;