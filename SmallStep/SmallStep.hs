module SmallStep
where

import Types
import AExp
import CExp
import qualified Data.Map as Map


run :: Program -> Memory -> Memory
run Skip m = m
run c m = run c' m'
            where (c', m') = stepCExp c m

