data Tree a = Node a (Tree a) (Tree a) | Empty
              deriving (Show)

treeHeight :: Tree a -> Int
treeHeight (Node _ t1 t2) = 1 + (max (treeHeight t1) (treeHeight t2))
treeHeight Empty          = 0
