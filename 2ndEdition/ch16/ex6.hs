-- Definition
data Tree = Leaf Int | Node Tree Tree

countLeaf :: Tree -> Int
countLeaf (Leaf _)   = 1
countLeaf (Node l r) = countLeaf l + countLeaf r

countNode :: Tree -> Int
countNode (Leaf _)   = 0
countNode (Node l r) = 1 + countNode l + countNode r

main = do
    print $ countLeaf (Leaf 100)
    print $ countNode (Leaf 100)
    print $ countLeaf (Node (Leaf 100) (Node (Leaf 200) (Leaf 300)))
    print $ countNode (Node (Leaf 100) (Node (Leaf 200) (Leaf 300)))

-- Proposition
(countLeaf t) - (countNode t) = 1

-- Base
(countLeaf (Leaf n)) - (countNode (Leaf n))
 -- Definition of countLeaf
 = 1 - (countNode (Leaf n))
 -- Definition of countLeaf
 = 1 - 0
 = 1

-- Recursion
if:
   countLeaf t - countNode t = 1

then:
(countLeaf (Node l r)) - (countNode (Node l r)) = 1
 -- Definition of countLeaf
 = countLeaf l + countLeaf r - (1 + countNode l + count Node r)
 = (countLeaf l - countNode l) + (countLeaf r - countNode r) - 1
 -- Assumption
 = 1 + 1 - 1
 = 1
 

