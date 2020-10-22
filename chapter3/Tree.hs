data Tree a = Node a (Tree a) (Tree a)
            | Empty
            deriving (Show)

data SimpleTree a = NodeS a (Maybe (SimpleTree a)) (Maybe (SimpleTree a))

height :: Tree a -> Int
height Empty = 0
height (Node a x y) = 1 + max (height x) (height y)