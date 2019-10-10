module Types
where

import qualified Data.Map as Map

type Memory = Map.Map String Integer
type Identifier = String

data Exp = Const Integer 
  | Id Identifier
  | Add Exp Exp
  | Div Exp Exp deriving (Show, Eq)
  
data BooleanExp = TRUE
  | FALSE
  | Not BooleanExp
  | And BooleanExp BooleanExp
  | Or BooleanExp BooleanExp 
  | GTT Exp Exp deriving (Show, Eq)

data Command = Skip
  | Atrib Identifier Exp
  | If BooleanExp Command Command 
  | Seq Command Command
  | While BooleanExp Command deriving (Show, Eq) 
