module AExp
where

import Types
import qualified Data.Map as Map
                            
stepAExp :: AExp -> Memory -> AExp
stepAExp (Var v) =  Num . Map.findWithDefault (-1) v
stepAExp (Som a b) = stepSom (Som a b)
stepAExp (Sub a b) = stepSub (Sub a b)
stepAExp (Mul a b) = stepMul (Mul a b)


stepSom :: AExp -> Memory -> AExp 
stepSom (Som (Num c1)(Num c2)) m = Num $ (+) c1 c2 
stepSom (Som (Num c) e) m = (Som (Num c) e') 
                                where e' = stepAExp e m
stepSom (Som e1 e2) m = (Som e1' e2)
                          where e1' = stepAExp e1 m

stepSub :: AExp -> Memory -> AExp 
stepSub (Sub (Num c1)(Num c2)) m = Num $ (-) c1 c2 
stepSub (Sub (Num c) e) m = (Sub (Num c) e') 
                          where e' = stepAExp e m
stepSub (Sub e1 e2) m = (Sub e1' e2)
                          where e1' = stepAExp e1 m

stepMul :: AExp -> Memory -> AExp
stepMul (Mul (Num a) (Num b)) _ = (Num $ (*) a b)
stepMul (Mul (Num a) e) m  = (Mul (Num a) e') where e' = stepAExp e m
stepMul (Mul e1 e2) m = (Mul e1' e2) where e1' = stepAExp e1 m