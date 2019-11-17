module CExp
where

import Types
import BExp
import AExp
import qualified Data.Map as Map


stepCExp :: CExp -> Memory -> (CExp, Memory)
stepCExp (Atrib id e) m = stepAtrib (Atrib id e) m
stepCExp (If b c1 c2) m = stepIf (If b c1 c2) m
stepCExp (Seq c1 c2) m = stepSeq (Seq c1 c2) m 
stepCExp (While b c1) m = stepWhile (While b c1) m

insert :: AExp -> Memory -> AExp -> Memory
insert (Var id) m (Num c) = Map.insert id c m
insert _ _ _ = error "Invalid atribution"

stepAtrib :: CExp -> Memory -> (CExp, Memory)
stepAtrib (Atrib id (Num c)) m = (Skip, insert id m (Num c))
stepAtrib (Atrib id e) m = (Atrib id c', m)
                            where c' = stepAExp e m

stepIf :: CExp -> Memory -> (CExp, Memory)
stepIf (If TRUE c _) m = (c, m)
stepIf (If FALSE _ c) m = (c, m)
stepIf (If b c1 c2) m = ((If b' c1 c2), m) 
                  where b' = stepBExp b m

stepSeq :: CExp -> Memory -> (CExp, Memory)
stepSeq (Seq Skip c) m = (c, m)
stepSeq (Seq c1 c2) m = ((Seq c1' c2), m') where (c1', m') = stepCExp c1 m

stepWhile :: CExp -> Memory -> (CExp, Memory)
stepWhile (While b c) m = ((If b (Seq c (While b c)) Skip), m)

