module AExp
where

import Types
import qualified Data.Map as Map
                            
stepAExp :: AExp -> Memory -> AExp
stepAExp (Num c) _ = Num c
stepAExp (Var v) m =  Num $ Map.findWithDefault (-1) v m
stepAExp (Som a b) m = stepSom (Som a b) m
stepAExp (Sub a b) m = stepSub (Sub a b) m
stepAExp (Mul a b) m = stepMul (Mul a b) m


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