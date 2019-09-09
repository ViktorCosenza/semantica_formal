module Main
where 

import BigStepSemantic
import Types
import qualified Data.Map as Map

main = do
  let memory = Map.fromList [("zero", 11)]
  print $ 
    run (
      If (GTT (Const 11) $ Id "zero") 
        (Atrib "true" $ Const 1) 
        (Atrib "false" $ Const 0)
    ) 
    memory