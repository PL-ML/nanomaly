
let rec digitsOfInt n =
  match n with | 0 -> [] | _ -> [n mod 10; digitsOfInt (n / 10)];;