data Tree a = Leaf a | Node (Tree a) (Tree a)

leaves :: Tree a-> Int
leaves (Leaf _) = 1
leaves (Node left right) = (leaves left) + (leaves right)

balanced :: Tree a -> Bool
balanced (Leaf _) = True
balanced (Node left right) = abs((leaves left) - (leaves right)) < 2
                             && balanced left
                             && balanced right

main = do
    print $ balanced (Leaf 1)
    print $ balanced (Node (Leaf 1) (Leaf 2))
    print $ balanced (Node (Leaf 1) (Node (Leaf 2) (Leaf 3)))
    print $ balanced (Node (Leaf 1) (Node (Leaf 2) (Node (Leaf 3) (Leaf 4))))
