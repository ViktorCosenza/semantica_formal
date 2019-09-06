
data Tree = Empty | Node a (Tree a) (Tree a) deriving (Show, Eq)

insert :: a -> Tree a -> Tree a
insert a Empty = Node a (Tree Empty) (Tree Empty)
insert a Node b  (Tree left) (Tree right)
    | a > b = insert a $ Tree left
    | a < b = insert a $ Tree right
    | otherwise = Tree Node b
    
