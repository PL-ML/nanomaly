
let sqsum xs = let f a x = (+.) in let base = 0 in List.fold_left f base xs;;
