module Main
where 

import BigStepSemantic
import Types
import qualified Data.Map as Map

main = do
  let memory = Map.fromList [("zero", 0)]
  print $ 
    run (
      While (GTT (Const 11) $ Id "zero") 
        (Atrib "zero" $ Add (Const 1) (Id "zero")) 
    ) 
    memory