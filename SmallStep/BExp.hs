module BExp
where

import Types
import AExp

stepBExp :: BExp -> Memory -> BExp
stepBExp (Not b) = stepNot (Not b)
stepBExp (And b1 b2) = stepAnd (And b1 b2)
stepBExp (Or b1 b2) = stepOr (Or b1 b2)
stepBExp (Ig e1 e2) = stepIg (Ig e1 e2)

stepNot :: BExp -> Memory -> BExp
stepNot (Not TRUE) _ = FALSE
stepNot (Not FALSE) _ = TRUE
stepNot (Not b) m = (Not b') where b' = stepBExp b m

stepAnd :: BExp -> Memory -> BExp
stepAnd (And FALSE _) _  = FALSE
stepAnd (And TRUE b) _ = b
stepAnd(And b1 b2) m = (And b1' b2) where b1' = stepBExp b1 m  

stepOr :: BExp -> Memory -> BExp
stepOr (Or TRUE _) _ = TRUE
stepOr (Or FALSE b) _ = b
stepOr(Or b1 b2) m = (Or b1' b2) where b1' = stepBExp b1 m 

stepIg :: BExp -> Memory -> BExp
stepIg (Ig (Num a) (Num b)) _ = if (==) a b then TRUE else FALSE
stepIg (Ig (Num n) e) m = (Ig (Num n) e') where e' = stepAExp e m 
stepIg (Ig e1 e2) m = (Ig e1' e2) where e1' = stepAExp e1 m