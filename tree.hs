
data Tree = Empty | Node Int Tree Tree deriving (Show, Eq)

insereTree :: Int -> Tree -> Tree
insereTree num Empty = Node num Empty Empty
insereTree num (Node n left right) 
  | num > n   = Node n left  $ insereTree num right
  | num < n   = Node n right $ insereTree num left
  | otherwise = Node n left right

multTree :: Int -> Tree -> Tree
multTree _ Empty                   = Empty
multTree m (Node num left right)   = (Node $ (*) m num) (multTree m left) (multTree m right)


