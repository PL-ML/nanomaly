
type expr =
  | VarX
  | VarY
  | Sine of expr
  | Cosine of expr
  | Square of expr
  | Hoi of expr* expr* expr
  | Average of expr* expr
  | Times of expr* expr
  | Thresh of expr* expr* expr* expr;;

let rec exprToString e =
  let exp = exprToString in
  match e with
  | VarX  -> "x"
  | VarY  -> "y"
  | Sine a -> "sin(pi*" ^ ((exp a) ^ ")")
  | Cosine a -> "cos(pi*" ^ ((exp a) ^ ")")
  | Square a -> ("(" ^ ((exp a) ^ ("*" ^ ((exp a) ^ ")")))) "/2"
  | Average (a,b) -> "((" ^ ((exp a) ^ ("+" ^ ((exp b) ^ ")/2)")))
  | Times (a,b) -> (exp a) ^ ("*" ^ (exp b))
  | Thresh (a,b,c,d) ->
      "(" ^
        ((exp a) ^
           ("<" ^ ((exp b) ^ ("?" ^ ((exp c) ^ (":" ^ ((exp d) ^ ")")))))))
  | Hoi (a,b,c) ->
      "sin(pi*" ^
        ((exp a) ^ (")*cos(pi*" ^ ((exp b) ^ (")/(" ^ ((exp c) ^ ")")))));;