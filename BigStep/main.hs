module Main
where 

import BigStepSemantic
import Types
import qualified Data.Map as Map

main = do
  let memory = Map.fromList [("zero", 0)]
  print $ 
    run (
      Seq
      (While (GTT (Const 1000) $ Id "zero") 
        (Atrib "zero" $ Add (Const 1) (Id "zero")))
      (
      Seq
        (Atrib "test2" (Add (Id "zero")(Const 44)))
        (
          If (Or FALSE FALSE)
          (Atrib "TRUE" (Const 32))
          (Atrib "FALSE" (Const 1111))
        ) 
      )
    ) 
    memory
