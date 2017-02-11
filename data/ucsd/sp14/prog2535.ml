
let pipe fs =
  let f a x = match a with | [] -> x | h::t -> h x in
  let base = 0 in List.fold_left f base fs;;
