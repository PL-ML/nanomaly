
let removeDuplicates l =
  let rec helper (seen,rest) =
    match rest with
    | [] -> seen
    | h::t ->
        let seen' = seen in
        (if (List.mem h List.rev t) = false then h :: seen';
         (let rest' = t in helper (seen', rest'))) in
  List.rev (helper ([], l));;