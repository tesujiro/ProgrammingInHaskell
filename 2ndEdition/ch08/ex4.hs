data Tree a = Leaf a | Node (Tree a) (Tree a)
              deriving (Eq, Ord, Show, Read)

balance :: [a] -> Tree a
balance[x] = Leaf x
balance xs = Node (balance (take l xs)) (balance (drop l xs))
               where l = length(xs) `div` 2

main = do
    print $ balance [1,2]
    print $ balance [1,2,3,4,5]
