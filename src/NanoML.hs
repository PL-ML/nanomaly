{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PartialTypeSignatures #-}
module NanoML where

import Control.Applicative
import Control.Monad.Catch hiding (try)
import Control.Monad.IO.Class
import qualified Data.HashSet as HashSet
import Data.Functor
import Data.List
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Typeable
import Text.Parser.Expression
import Text.Parser.Token.Style
import Text.Trifecta

import Debug.Trace
import Text.Printf

----------------------------------------------------------------------
-- Core Types
----------------------------------------------------------------------

type Var = String

type Env = Map Var Value

insertEnv :: Var -> Value -> Env -> Env
insertEnv = Map.insert

-- | Left-biased union of environments.
joinEnv :: Env -> Env -> Env
joinEnv = Map.union

lookupEnv :: MonadThrow m => Var -> Env -> m Value
lookupEnv v env = case Map.lookup v env of
  Nothing -> throwM (UnboundVariable v)
  Just x  -> return x

emptyEnv :: Env
emptyEnv = Map.empty

data UnboundVariable
  = UnboundVariable Var
  deriving (Show, Typeable)
instance Exception UnboundVariable

data Value
  = VI Int
  | VD Double
  | VS String
  | VB Bool
  | VU
  | VL [Value]
  | VT Int [Value] -- VT sz:{Nat | sz >= 2} (ListN Value sz)
  | VC (Maybe Var) Env Pat Expr
  deriving (Show)

data Literal
  = LI Int
  | LD Double
  | LB Bool
  | LS String
  | LU
  deriving (Show)

data RecFlag = Rec | NonRec deriving (Show)

data Decl
  = Decl RecFlag Pat Expr
  deriving (Show)

data Expr
  = Var Var
  | Lam Pat Expr
  | App Expr Expr
  | Bop Bop Expr Expr
  | Lit Literal
  | Let RecFlag Pat Expr Expr
  | Ite Expr Expr Expr
  | Seq Expr Expr -- TODO: do we actually need this for the student examples?
  | Case Expr [Alt]
  | Cons Expr Expr
  | Nil
  | Tuple [Expr]
  deriving (Show)

data ExprF f
  = VarF Var
  | LamF Var f
  | AppF f f
  | BopF Bop f f
  | LitF Literal
  | LetF RecFlag Pat f f
  | IteF f f f
  | SeqF f f -- TODO: do we actually need this for the student examples?
  | CaseF f [(Pat, ExprF f)]
  | ConsF f f
  | NilF
  | TupleF [f]
  deriving (Show, Functor)

--type LocExpr = Fix ExprF Careted

data Bop
  = Eq | Neq
  | Lt | Le
  | Gt | Ge
  | Plus  | Minus  | Times  | Div
  | FPlus | FMinus | FTimes | FDiv
  deriving (Show)

type Alt = (Pat, Expr)

data Pat
  = VarPat Var
  | LitPat Literal
  | ConsPat Pat Pat
  | ListPat [Pat]
  | TuplePat [Pat]
  | FunPat Var [Pat] -- TODO: should this be `[Pat]`?
  | WildPat
  deriving (Show)

instance Exception [Char]

----------------------------------------------------------------------
-- Evaluation
----------------------------------------------------------------------

evalDecl :: MonadThrow m => Decl -> Env -> m Env
evalDecl decl env = case decl of
  Decl Rec (FunPat f ps) e -> do
    c <- setClosureName f <$> eval (mkCurried ps e) env
    return (insertEnv f c env)
  Decl NonRec (FunPat f ps) e -> do
    c <- eval (mkCurried ps e) env
    return (insertEnv f c env)

eval :: MonadThrow m => Expr -> Env -> m Value
eval expr env = traceShow expr $ case expr of
  Var v ->
    lookupEnv v env
  Lam v e ->
    return (VC Nothing env v e)
  App e1 e2 -> do
    v1 <- eval e1 env
    v2 <- eval e2 env
    evalApp v1 v2
  Bop b e1 e2 -> do
    v1 <- eval e1 env
    v2 <- eval e2 env
    evalBop b v1 v2
  Lit l -> return (litValue l)
  Let Rec (FunPat f ps) e b -> do
    c <- setClosureName f <$> eval (mkCurried ps e) env
    eval b (insertEnv f c env)
  Let NonRec (FunPat f ps) e b -> do
    c <- eval (mkCurried ps e) env
    eval b (insertEnv f c env)
  Let NonRec p e b ->
    eval (Case e [(p,b)]) env
  Ite eb et ef -> do
    vb <- eval eb env
    case vb of
      VB True  -> eval et env
      VB False -> eval ef env
      _        -> throwM "if-then-else given a non-boolean"
  Seq e1 e2 ->
    eval e1 env >> eval e2 env
  Case e as -> do
    v <- eval e env
    traceM $ "matching: " ++ show v
    traceM $ "against:  " ++ show as
    evalAlts v as env
  Cons e es -> do
    v     <- eval e env
    VL vs <- eval es env
    return (VL (v : vs))
  Nil -> return (VL [])
  Tuple es -> do
    vs <- mapM (`eval` env) es
    return (VT (length vs) vs)

evalApp :: MonadThrow m => Value -> Value -> m Value
evalApp f a = case f of
  VC Nothing env p e -> do
    Just pat_env <- matchPat a p
    eval e (joinEnv pat_env env)
  VC (Just me) env p e -> do
    Just pat_env <- matchPat a p
    eval e (joinEnv pat_env (insertEnv me f env))
  _ -> throwM "tried to apply a non-function"

evalBop :: MonadThrow m
        => Bop -> Value -> Value -> m Value
evalBop Eq  v1 v2 = VB <$> eqVal v1 v2
evalBop Neq v1 v2 = VB . not <$> eqVal v1 v2
evalBop Lt  v1 v2 = VB <$> ltVal v1 v2
evalBop Le  v1 v2 = (\x y -> VB (x || y)) <$> ltVal v1 v2 <*> eqVal v1 v2
evalBop Gt  v1 v2 = VB <$> gtVal v1 v2
evalBop Ge  v1 v2 = (\x y -> VB (x || y)) <$> gtVal v1 v2 <*> eqVal v1 v2
evalBop Plus v1 v2 = VI <$> plusVal v1 v2
evalBop Minus v1 v2 = VI <$> minusVal v1 v2
evalBop Times v1 v2 = VI <$> timesVal v1 v2
evalBop Div v1 v2 = VI <$> divVal v1 v2

eqVal (VI x) (VI y) = return (x == y)
eqVal (VD x) (VD y) = return (x == y)
eqVal (VB x) (VB y) = return (x == y)
eqVal VU     VU     = return True
eqVal (VL x) (VL y) = and . ((length x == length y) :) <$> zipWithM eqVal x y
eqVal (VT i x) (VT j y)
  | i == j
  = and <$> zipWithM eqVal x y
eqVal x y
  = throwM (printf "cannot check incompatible types for equality: (%s) (%s)" (show x) (show y) :: String)

ltVal (VI x) (VI y) = return (x < y)
ltVal (VD x) (VD y) = return (x < y)
ltVal x      y      = throwM "cannot compare ordering of non-numeric types"

gtVal (VI x) (VI y) = return (x > y)
gtVal (VD x) (VD y) = return (x > y)
gtVal x      y      = throwM "cannot compare ordering of non-numeric types"

plusVal (VI i) (VI j) = return (i+j)
plusVal _      _      = throwM "+ can only be applied to ints"

minusVal (VI i) (VI j) = return (i-j)
minusVal _      _      = throwM "- can only be applied to ints"

timesVal (VI i) (VI j) = return (i*j)
timesVal _      _      = throwM "* can only be applied to ints"

divVal (VI i) (VI j) = return (i `div` j)
divVal _      _      = throwM "/ can only be applied to ints"


litValue :: Literal -> Value
litValue (LI i) = VI i
litValue (LD d) = VD d
litValue (LB b) = VB b
litValue (LS s) = VS s
litValue LU     = VU

evalAlts :: MonadThrow m => Value -> [Alt] -> Env -> m Value
evalAlts _ [] _
  = throwM "no matching pattern"
evalAlts v ((p,e):as) env
  = matchPat v p >>= \case
      Nothing  -> evalAlts v as env
      Just bnd -> eval e (joinEnv bnd env)

-- | If a @Pat@ matches a @Value@, returns the @Env@ bound by the
-- pattern.
matchPat :: MonadThrow m => Value -> Pat -> m (Maybe Env)
-- FIXME: should be MonadThrow m => m (Maybe Env) so we can throw exception on ill-typed pattern match
matchPat v p = case p of
  VarPat var ->
    return $ Just (insertEnv var v emptyEnv)
  LitPat lit ->
    return $ if matchLit v lit then Just emptyEnv else Nothing
  ConsPat p ps -> do
    (x,xs) <- unconsVal v
    Just env1 <- matchPat x p
    Just env2 <- matchPat xs ps
    return $ Just (joinEnv env1 env2)
  ListPat ps -> throwM "matchPat.ListPat"
  TuplePat ps -> throwM "matchPat.TuplePat"
  FunPat _ _ -> throwM "cannot pattern-match on function"
  WildPat ->
    return $ Just emptyEnv

unconsVal :: MonadThrow m => Value -> m (Value, Value)
unconsVal (VL (x:xs)) = return (x, VL xs)
unconsVal _           = throwM "type error: uncons can only be applied to lists"

matchLit :: Value -> Literal -> Bool
matchLit (VI i1) (LI i2) = i1 == i2
matchLit (VD d1) (LD d2) = d1 == d2
matchLit (VB b1) (LB b2) = b1 == b2
matchLit VU      LU      = True
matchLit _       _       = False

mkCurried :: [Pat] -> Expr -> Expr
mkCurried [p]    e = Lam p e
mkCurried (p:ps) e = Lam p (mkCurried ps e)

setClosureName :: Var -> Value -> Value
setClosureName f (VC _ r v e) = VC (Just f) r v e

mkList :: [Expr] -> Expr
mkList = foldr Cons Nil

----------------------------------------------------------------------
-- Parsing
----------------------------------------------------------------------

idStyle :: IdentifierStyle Parser
idStyle = emptyIdents { _styleReserved
                        = HashSet.fromList
                          [ "let", "rec", "in", "if", "then", "else"
                          , "match", "with", "end", "fun", "function"
                          , "true", "false"
                          ]
                      , _styleLetter
                        = _styleLetter emptyIdents <|> char '.'
                      }

opStyle :: IdentifierStyle Parser
opStyle = emptyOps { _styleReserved
                     = HashSet.fromList
                       [ "+", "-", "*", "/", "+.", "-.", "*.", "/."
                       , "++", "^", "="
                       ]
                   }

identifier :: Parser Var
identifier = ident idStyle

reserved :: String -> Parser ()
reserved = reserve idStyle

reservedOp :: String -> Parser ()
reservedOp = reserve idStyle

decl :: Parser Decl
decl = do whiteSpace
          reserved "let"
          r <- Rec <$ reserved "rec" <|> return NonRec
          p <- pat
          reservedOp "="
          e <- expr
          reservedOp ";;"
          return (Decl r p e)

expr :: Parser Expr
expr =   buildExpressionParser table compound
     <?> "expression"

compound :: Parser Expr
compound =   let_ <|> ite <|> match <|> app
         <?> "compound expression"

let_ :: Parser Expr
let_ = do reserved "let"
          r <- Rec <$ reserved "rec" <|> return NonRec
          p <- pat
          reservedOp "="
          e <- expr
          reserved "in"
          body <- expr
          return (Let r p e body)

match :: Parser Expr
match = do reserved "match"
           e <- expr
           reserved "with"
           as <- many alt
           return (Case e as)
      <?> "match"

alt :: Parser Alt
alt = do reservedOp "|"
         p <- pat
         reservedOp "->"
         e <- expr
         return (p,e)

pat :: Parser Pat
pat = buildExpressionParser [[ Infix (ConsPat <$ reservedOp "::") AssocRight ]] simplePat

simplePat :: Parser Pat
simplePat =   WildPat <$ reserved "_"
          <|> LitPat <$> literal
          <|> ListPat <$> listOf pat
          <|> TuplePat <$> tupleOf pat
          <|> parens pat
          <|> funVarPat

funVarPat :: Parser Pat
funVarPat = do v  <- identifier
               ps <- try $ many pat
               if null ps
                 then return (VarPat v)
                 else return (FunPat v ps)

ite :: Parser Expr
ite = do reserved "if"
         b <- expr
         reserved "then"
         t <- expr
         reserved "else"
         e <- expr
         return (Ite b t e)

app :: Parser Expr
app = do f    <- term
         args <- many term
         return (foldl' App f args)
      <?> "function application"

term :: Parser Expr
term =   Lit <$> literal
     <|> Var <$> var
     <|> mkList <$> listOf expr
     <|> Tuple <$> try (tupleOf expr)
     <|> parens expr
     <?> "simple expression"

var :: Parser Var
var = identifier

literal :: Parser Literal
literal =   either (LI . fromInteger) LD <$> try integerOrDouble
        <|> LB <$> bool
        <|> LS <$> stringLiteral
        <|> LU <$ reservedOp "()"
        <?> "literal"

bool :: Parser Bool
bool =   True  <$ reserved "true"
     <|> False <$ reserved "false"
     <?> "boolean"

table :: [[Operator Parser Expr]]
table  = [ [ binary "=" Eq AssocNone, binary "<>" Neq AssocNone ]
         , [ binary "*"  Times  AssocLeft, binary "/"  Div  AssocLeft
           , binary "*." FTimes AssocLeft, binary "/." FDiv AssocLeft
           ]
         , [ binary "+"  Plus  AssocLeft, binary "-"   Minus  AssocLeft
           , binary "+." FPlus AssocLeft, binary "-."  FMinus AssocLeft
           ]
         , [ infix_ "::" Cons AssocRight ]
         ]

binary :: String -> Bop -> Assoc -> Operator Parser Expr
binary name bop = Infix (Bop bop <$ reservedOp name)

infix_ :: String -> (Expr -> Expr -> Expr) -> Assoc -> Operator Parser Expr
infix_ name op = Infix (op <$ reservedOp name)

-- prefix  name fun       = Prefix (fun <* reservedOp name)
-- postfix name fun       = Postfix (fun <* reservedOp name)

listOf :: Parser a -> Parser [a]
listOf p = brackets (p `sepBy` semi)

tupleOf :: Parser a -> Parser [a]
tupleOf p = parens $ do
  -- make sure we get at least two elemets in the tuple
  x  <- p <* comma
  xs <- p `sepBy1` comma
  return (x:xs)


----------------------------------------------------------------------
-- Utilities
----------------------------------------------------------------------

evalString :: MonadThrow m => String -> m Value
evalString s = case parseString expr mempty s of
  Success e -> eval e emptyEnv

facProg :: String
facProg = unlines [ "let rec fac n ="
                  , "  if n = 0 then"
                  , "    1"
                  , "  else"
                  , "    n * fac (n - 1)"
                  , ";;"
                  ]

badProg :: String
badProg = unlines [ "let f lst ="
                  , "  let rec loop lst acc ="
                  , "    if lst = [] then"
                  , "      acc"
                  , "    else"
                  , "      ()"
                  , "  in"
                  , "  match loop lst [(0.0,0.0)] with"
                  , "    | h :: t -> h"
                  , ";;"
                  ]

zipWithM :: Monad m => (a -> b -> m c) -> [a] -> [b] -> m [c]
zipWithM f as bs = sequence (zipWith f as bs)

parseTopLevel :: String -> Result [Decl]
parseTopLevel = parseString (many decl <* eof) mempty

parseTopLevelFile :: MonadIO m => FilePath -> m (Result [Decl])
parseTopLevelFile = parseFromFileEx (many decl <* eof)
