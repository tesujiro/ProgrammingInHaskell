data Tree a = Leaf a | Node (Tree a) a (Tree a)
              deriving (Eq, Ord, Show, Read)

occurs :: Ord a => a -> Tree a -> Bool
occurs x (Leaf y) = x == y
occurs x (Node l y r) | x == y = True
                      | x < y = occurs x l
                      | otherwise = occurs x r

occurs' :: Ord a => a -> Tree a -> Bool
occurs' x (Leaf y) = x==y
occurs' x (Node l y r) = case compare x y of
                            EQ -> True
                            LT -> occurs' x l
                            GT -> occurs' x r


t :: Tree Int
t = Node (Node (Leaf 1) 3 (Leaf 4)) 5 (Node (Leaf 6) 7 (Leaf 9))

main = do 
    print $ t
    print $ occurs 10 t
    print $ occurs 7 t
    print $ occurs' 10 t
    print $ occurs' 7 t
