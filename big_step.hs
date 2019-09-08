import qualified Data.Map as Map

type Id = String
type Memory = Map.Map String Integer

data Exp = Const Integer 
    | Id
    | Add Exp Exp
    | Div Exp Exp deriving (Show, Eq)

data Command = Skip
    | Atrib Id Exp
    | Seq Command Command deriving (Show, Eq)


safeDiv :: Exp -> Exp -> Maybe Integer
safeDiv _ (Const 0) = Nothing
safeDiv a b = do
            x <- eval a
            y <- eval b
            return $ quot x y

add :: Exp -> Exp -> Maybe Integer
add a b = do
    x <- eval a
    y <- eval b
    return $ x + y

eval :: Exp -> Maybe Integer
eval (Const a) = return a
eval (Div a b) = safeDiv a b
eval (Add a b) = add a b

atrib :: String -> Exp -> Memory -> Maybe Memory 
atrib id exp m = do
    result <- eval exp
    Just $ Map.insert id result m

seqCommand :: Command -> Command -> Memory -> Maybe Memory
seqCommand c1 c2 m = do
    m' <- run c1 m
    run c2 m'

run :: Command -> Memory -> Maybe Memory
run Skip m           = Just m
run (Atrib id exp) m = atrib id exp m
run (Seq c1 c2) m    = seqCommand c1 c2 m 

main = do
    let memory = Map.fromList [("zero", 0)]
    print $ run (Seq (Atrib "teste" $ Const 1) $ Atrib "teste2" (Const 2)) memory