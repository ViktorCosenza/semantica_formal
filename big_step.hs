import qualified Data.Map as Map

type Memory = Map.Map String Integer

type Identifier = String

data Exp = Const Integer 
  | Id Identifier
  | Add Exp Exp
  | Div Exp Exp deriving (Show, Eq)

data BooleanExp = TRUE
  | FALSE
  | And BooleanExp BooleanExp 
  | GTT Exp Exp deriving (Show, Eq)

data Command = Skip
  | Atrib Identifier Exp
  | If BooleanExp Command Command 
  | Seq Command Command deriving (Show, Eq) 


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

evalBool :: BooleanExp -> Memory ->BooleanExp
evalBool TRUE _       = TRUE
evalBool FALSE _      = FALSE
evalBool (And e1 e2) m = andBool e1 e2 m
evalBool (GTT e1 e2) m = greaterThan e1 e2 m

atrib :: String -> Exp -> Memory -> Maybe Memory 
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

run :: Command -> Memory -> Maybe Memory
run Skip m            = Just m
run (Atrib id exp) m  = atrib id exp m
run (If b c1 c2) m    = ifThenElse b c1 c2 m
run (Seq c1 c2) m     = seqCommand c1 c2 m 

main = do
  let memory = Map.fromList [("zero", 11)]
  print $ 
    run (
      If (GTT (Const 11) $ Id "zero") 
        (Atrib "true" $ Const 1) 
        (Atrib "false" $ Const 0)) 
        memory