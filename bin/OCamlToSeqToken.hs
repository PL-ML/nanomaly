import Data.Aeson hiding (Array)
import qualified Data.ByteString.Lazy.Char8 as LBC
import NanoML
import NanoML.Types (Expr, Prog, Pat, Literal, Bop, Uop, primBops)

main :: IO ()
main = interact ocamlToSeq

ocamlToSeq :: String -> String
ocamlToSeq inp = case parseTopForm inp of
  Left e -> error e
  Right ds -> show $ toTokens ds  
  
-- concat $ insertSep ", "  $ toTokens ds


insertSep :: a -> [a] -> [a]
insertSep sep strs = case strs of
  [] -> []
  [x] -> [x]
  x:xs -> [x, sep] ++ insertSep sep xs


uopToTokens :: Uop -> [String]
uopToTokens Neg = ["-"]
uopToTokens FNeg = ["-."]

bopToTokens :: Bop -> [String]
bopToTokens op = findIn primBops
  where 
    findIn bops = case bops of
      [] -> error (show op ++ " seems to an invalid bop")
      (name, bop) : xs ->  if op == bop then [name] else findIn xs


litToTokens :: Literal -> [String]
litToTokens l = case l of
    LI i -> [ show i ] -- [ show i, ":", "int"]
    LD d -> [ show d ] -- [ show d, ":", "double"]
    LB b -> [ show b ] -- [ show b, ":", "bool"]
    LC c -> [ show c ] -- [ show c, ":", "char"]
    LS s -> [ show s ] -- [ show s, ":", "string"]


patToTokens :: Pat -> [String]
patToTokens pat = case pat of
    VarPat _ v ->  [v] -- Aeson.object ["var" .= v]
    LitPat _ l ->  litToTokens l -- Aeson.object ["lit" .= l]
    IntervalPat _ x y -> error "paToToken IntervalPat TBD" -- Aeson.object ["interval" .= [x, y]]
    ConsPat _ x y -> error "paToToken ConsPat TBD" -- Aeson.object ["cons" .= [x, y]]
    ConPat _ x y -> error "paToToken ConPat TBD" -- Aeson.object ["con" .= [toJSON x, toJSON y]]
    ListPat _ xs -> error "paToToken ListPat TBD" -- Aeson.object ["list" .= xs]
    TuplePat _ xs -> error "paToToken TuplePat TBD" -- Aeson.object ["tuple" .= xs]
    WildPat _ -> error "paToToken WildPat TBD" -- Aeson.object ["wild" .= ()]
    OrPat _ x y -> error "paToToken OrPat TBD" -- Aeson.object ["or" .= [x, y]]
    AsPat _ x y -> error "paToToken AsPat TBD" -- Aeson.object ["as" .= [toJSON x, toJSON y]]
    ConstraintPat _ x y -> error "paToToken ConstraintPat TBD" --  Aeson.object ["type" .= [toJSON x, toJSON y]]


mapSepConcat :: String -> [Expr] -> [String]
mapSepConcat sep es  = concat $ insertSep [sep] (map exprToTokens es)

-- the inverse process of  src/NanoML/parser.y  starting from line:258
exprToTokens :: Expr -> [String]
exprToTokens e = case e of
    Var _ v           -> [v] -- ["var" .= v]
    Lam _ p e _       ->  ["fun"] ++ patToTokens p ++ ["->"] ++ exprToTokens e  -- ["fun" .= Aeson.object ["pat" .= p, "exp" .= e]]
    App _ f es        ->  ["("] ++ exprToTokens f  ++ concatMap exprToTokens es ++ [")"]  -- ["app" .= Aeson.object ["fun" .= f, "args" .= es]]
    Bop _ b x y       ->  ["("] ++ exprToTokens x ++ bopToTokens b  ++ exprToTokens y ++ [")"] -- ["bop" .= Aeson.object ["op" .= b, "left" .= x, "right" .= y]]
    Uop _ u x         ->  uopToTokens u ++  exprToTokens x -- ["uop" .= Aeson.object ["op" .= u, "arg" .= x]]
    Lit _ l           -> litToTokens l   -- ["lit" .= l]
    Let _ Rec ps e    -> ["let", "rec"] ++ concatMap patExprToToken ps ++ ["in"] ++ exprToTokens e  -- ["letrec" .= Aeson.object ["binds" .= ps, "body" .= e]]
    Let _ NonRec ps e -> ["let"] ++ concatMap patExprToToken ps ++ ["in"] ++ exprToTokens e  -- ["let" .= Aeson.object ["binds" .= ps, "body" .= e]]
    Ite _ b t f       ->  ["if"] ++ exprToTokens b ++ ["then"] ++ exprToTokens t ++ ["else"] ++ exprToTokens f -- ["ite" .= Aeson.object ["cond" .= b, "then" .= t, "else" .= f]]
    Seq _ x y         -> exprToTokens x ++ [";"] ++ exprToTokens y -- ["seq" .= [x, y]]
    Tuple _ es        -> ["("] ++ mapSepConcat "," es ++ [")"] -- ["tuple" .= es]
    List _ es _       -> ["["] ++ mapSepConcat ";" es ++ ["]"]  -- ["list" .= es]
    Case _ e alts     ->  error "exprToTokens, Case TBD" -- ["case" .= Aeson.object ["exp" .= e, "alts" .= alts]]
    ConApp _ d me _   -> --  ["con" .= [toJSON d, toJSON me]]
      case me of 
        Nothing -> ["[]"] 
        Just (Tuple _ es) ->  mapSepConcat "::" es
        Just e -> ["["] ++ exprToTokens e ++ ["]"]
    Record _ fs _     -> error "exprToTokens, Record TBD" -- ["record" .= fs]
    Field _ e f       -> error "exprToTokens, Field TBD"-- ["getField" .= Aeson.object ["exp" .= e, "fld" .= f]]
    SetField _ e f v  -> error "exprToTokens, SetField TBD" -- ["setField" .= Aeson.object ["exp" .= e, "fld" .= f, "val" .= v]]
    Array _ es _      -> error "exprToTokens, Array TBD" -- ["array" .= es]
    Try _ e alts      -> error "exprToTokens, Try TBD" -- ["try" .= Aeson.object ["exp" .= e, "with" .= alts]]
    _ -> error "exprTokens, other TBD!!"

-- (pat, expr)
patExprToToken :: (Pat, Expr) -> [String]
patExprToToken (p, e) = patToTokens p ++ ["="] ++ exprToTokens e


-- type Prog = [Decl]
toTokens :: Prog -> [String]
toTokens [] = []
toTokens (d:ds) = case d of
  DFun srcSpan NonRec pat_expr_list -> ["let"] ++  concatMap patExprToToken pat_expr_list ++ [";;\n"] ++ toTokens ds
  DFun srcSpan Rec pat_expr_list -> ["let", "rec"] ++  concatMap patExprToToken pat_expr_list ++ [";;\n"] ++ toTokens ds
  DEvl srcSpan e -> exprToTokens e ++ [";;\n"] ++ toTokens ds
  _ -> toTokens ds
