module Main
where 

import SmallStep
import Types
import qualified Data.Map as Map



testec1 :: CExp
testec1 = (Seq (Seq (Atrib (Var "z") (Var "x")) (Atrib (Var "x") (Var "y"))) 
		(Atrib (Var "y") (Var "z")))

testec2 :: CExp
testec2 = (If (Not TRUE)
            (Atrib (Var "x") (Num 1))
            (Atrib (Var "y") (Num 1)))

fatorial :: CExp
fatorial = (Seq (Atrib (Var "y") (Num 1))
                (While (Not (Ig (Var "x") (Num 1)))
                       (Seq (Atrib (Var "y") (Mul (Var "y") (Var "x")))
                            (Atrib (Var "x") (Sub (Var "x") (Num 1))))))

              

main = do
  let memory = Map.fromList [("x", 3)]
  print $ 
    SmallStep.run fatorial memory
