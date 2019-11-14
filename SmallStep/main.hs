module Main
where 

import SmallStep
import Types
import qualified Data.Map as Map

main = do
  let memory = Map.fromList [("zero", 0)]
  print $ SmallStep.run (Atrib "zero" (Add (Const 5) (Const 1))) memory
