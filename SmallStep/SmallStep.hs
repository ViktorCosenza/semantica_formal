module SmallStep
where

import Types
import qualified Data.Map as Map


insert :: Identifier -> Memory -> Exp -> Memory
insert id m (Const c) = Map.insert id c m


run :: Program -> Memory -> Memory
run Skip m = m
run c m = run c' m'
            where (c', m') = step c m


step :: Command -> Memory -> (Command, Memory)
step (Atrib id e) m = stepAtrib (Atrib id e) m
step _ _ = (Skip, Map.fromList [("aaaaa", 1)])

stepAtrib :: Command -> Memory -> (Command, Memory)
stepAtrib (Atrib id (Const c)) m = (Skip, insert id m (Const c))
stepAtrib (Atrib id e) m = (Atrib id c', m)
                            where c' = stepExp e m
stepExp :: Exp -> Memory -> Exp
stepExp (Const c) m = Const c 
stepExp e m = stepExp e' m
              where e' = stepAdd e m


stepAdd :: Exp -> Memory -> Exp 
stepAdd (Add (Const c1)(Const c2)) m = Const $ (+) c1 c2 
stepAdd (Add (Const c) e) m = (Add (Const c) e') 
                                where e' = stepExp e m
stepAdd (Add e1 e2) m = (Add e1' e2)
                          where e1' = stepExp e1 m
