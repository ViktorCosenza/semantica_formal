import Control.Monad
import Data.List.Split
import System.IO

import qualified Data.Map.Strict as Map

data Binary = Add 
  | Sub 
  | Div 
  | Mult deriving (Show, Eq)

data Command = (Atrib a b) | Skip deriving (Show)

data Instruction = CommandOp Command
  | BinaryOp Binary deriving (Show)

emptyMemory :: Memory
emptyMemory = Memory []

setMemory ::  Integer -> Integer -> Memory -> Memory



binaryOp :: Binary -> Integer -> Integer -> Integer
binaryOp Add = (+) 
binaryOp Sub = (-)
binaryOp Div = quot 
binaryOp Mult = (*)

commandOp :: Command -> Memory -> Memory
commandOp (Atrib a b) (Memory m) = 

step :: Instruction -> Memory -> Memory


run :: [Instruction] -> Memory -> Memory
run [] (Memory a) = Memory a


main = do
  print "Nothing to do"