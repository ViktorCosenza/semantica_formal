module Types
where

import qualified Data.Map as Map

type Memory = Map.Map String Int

data AExp = Num Int
    | Var String
    | Som AExp AExp
    | Sub AExp AExp
    | Mul AExp AExp
  deriving (Show, Eq)

data BExp = TRUE
     | FALSE
     | Not BExp
     | And BExp BExp
     | Or  BExp BExp
     | Ig  AExp AExp
   deriving (Show, Eq)

data CExp = While BExp CExp
     | If BExp CExp CExp
     | Seq CExp CExp
     | Atrib AExp AExp
     | Skip
   deriving (Show, Eq)   

type Program = CExp