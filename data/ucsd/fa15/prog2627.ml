
let identity a = a;;

let pipe fs =
  let f a x a y = x (a y) in let base = identity in List.fold_left f base fs;;

let _ = pipe [] 3;;