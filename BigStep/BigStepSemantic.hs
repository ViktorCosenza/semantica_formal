module BigStepSemantic
where

import Types
import qualified Data.Map as Map


safeDiv :: Exp -> Exp -> Memory -> Maybe Integer
safeDiv _ (Const 0) _ = Nothing
safeDiv a b m = do
  x <- eval a m
  y <- eval b m 
  return $ quot x y

add :: Exp -> Exp -> Memory -> Maybe Integer
add a b m = do
  x <- eval a m
  y <- eval b m
  return $ x + y

eval :: Exp -> Memory -> Maybe Integer
eval (Const a) _ = Just $ a
eval (Id a) m    = Just $ Map.findWithDefault (-1) a m
eval (Div a b) m = safeDiv a b m
eval (Add a b) m = add a b m

andBool :: BooleanExp -> BooleanExp -> Memory -> BooleanExp
andBool FALSE _ _ = FALSE
andBool TRUE b2 m = evalBool b2 m
andBool b1 b2 m   = andBool (evalBool b1 m) b2 m

greaterThan :: Exp -> Exp -> Memory -> BooleanExp
greaterThan e1 e2 m = if (>) (eval e1 m) $ eval e2 m then TRUE else FALSE

notBool :: BooleanExp -> BooleanExp
notBool TRUE  = FALSE
notBool FALSE = TRUE

evalBool :: BooleanExp -> Memory -> BooleanExp
evalBool TRUE _        = TRUE
evalBool FALSE _       = FALSE
evalBool (Not e1) m    = notBool (evalBool e1 m)
evalBool (And e1 e2) m = andBool e1 e2 m
evalBool (Or e1 e2) m  = notBool $ andBool (notBool (evalBool e1 m)) (notBool (evalBool e2 m)) m
evalBool (GTT e1 e2) m = greaterThan e1 e2 m

atrib :: Identifier -> Exp -> Memory -> Maybe Memory 
atrib id exp m = do
  result <- eval exp m
  Just $ Map.insert id result m

seqCommand :: Command -> Command -> Memory -> Maybe Memory
seqCommand c1 c2 m = do
  m' <- run c1 m
  run c2 m'

ifThenElse :: BooleanExp -> Command -> Command -> Memory -> Maybe Memory
ifThenElse TRUE c _  m = run c m
ifThenElse FALSE _ c m = run c m 
ifThenElse exp c1 c2 m = ifThenElse (evalBool exp m) c1 c2 m 

while :: BooleanExp -> Command -> Memory -> Maybe Memory
while b c m
  | evalBool b m == TRUE = run (Seq c $ While b c) m 
  | otherwise            = Just m

run :: Command -> Memory -> Maybe Memory
run Skip m            = Just m
run (Atrib id exp) m  = atrib id exp m
run (If b c1 c2) m    = ifThenElse b c1 c2 m
run (While b c) m     = while b c m
run (Seq c1 c2) m     = seqCommand c1 c2 m 
