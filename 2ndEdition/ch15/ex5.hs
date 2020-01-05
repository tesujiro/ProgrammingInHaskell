data Tree a = Leaf | Node (Tree a) a (Tree a)
              deriving Show

repeatTree :: a -> Tree a
repeatTree x = Node left x right
               where left  = Node left x right
                     right = Node left x right

takeTree :: Int -> Tree a -> Tree a
takeTree 0 _    = Leaf
takeTree _ Leaf = Leaf
takeTree n (Node left x right) = Node (takeTree (n-1) left) x (takeTree (n-1) right)

replicateTree :: Int -> a -> Tree a
replicateTree n = takeTree n . repeatTree

main = do
    print $ Node (Node Leaf 1 Leaf) 2 Leaf
    print $ takeTree 0 (repeatTree 1)
    print $ takeTree 3 (repeatTree 1)
    print $ replicateTree 3 True 
