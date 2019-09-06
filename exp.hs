data Exp = Num Int
    | Suc Exp
    | Pred Exp

data Binary = Suc Exp | Pred Exp

binaryOp :: Binary -> Int -> Int
binaryOp Suc = (+) 1
binaryOp Pred = (+) -1

run :: Exp -> Int
run (Num a) = a
run (Suc e) = (run e) + 1
run (Pred e) = (run e) - 1

main = do
    let exp = Suc $ Pred $ Suc $ Num 5
    let result = run exp
    print result