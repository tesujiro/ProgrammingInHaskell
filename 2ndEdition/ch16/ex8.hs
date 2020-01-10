data Tree a = Leaf a | Node (Tree a) (Tree a)

instance Functor Tree where
    -- fmap :: (a -> b) -> Tree a -> Tree b
    fmap g (Leaf x) = Leaf (g x)
    fmap g (Node l r) = Node (fmap g l) (fmap g r)

fmap :: (a -> b) -> Tree a -> Tree b
-- Proposition 1
fmap id      = id
-- Proposition 2
fmap (g . h) = fmap g . fmap h

-- Proposition 1
-- Base
fmap id (Leaf x)
 -- Definition
 = Leaf (id x)
 -- Definition of id
 = Leaf x
 -- Definition of id
 = id (Leaf x)

-- Recursion
if:
fmap id t = id t 

then:
fmap id (Node l r)
 -- Definition
 = Node (fmap id l) (fmap id r)
 -- Apply Assumption
 = Node (id l) (id r)
 -- id 
 = Node l r

-- Proposition 2
-- Base
fmap (g . h) (Leaf x)
 -- Definition
 = Leaf ((g . h) x)
 -- (.)
 = Leaf (g (h x))
 -- Reverse Definitionh
 = fmap g (Leaf (h x))
 -- Reverse Definitionh
 = fmap g (fmap h (Leaf x))
 -- (.)
 = (fmap g . fmap h) (Leaf x)

-- Recursion
if:
fmap (g . h) t = (fmap g . fmap h) t

then:
fmap (g . h) (Node l r)
 -- Definition
 = Node (fmap (g . h) l) (fmap (g . h) r)
 -- Apply Assumption
 = Node ((fmap g . fmap h) l) ((fmap g . fmap h) r)
 -- (.)
 = Node (fmap g (fmap h l)) (fmap g (fmap h r))
 -- Reverse Definition
 = fmap g (Node (fmap h l) (fmap h r))
 -- Reverse Definition
 = fmap g (fmap h (Node l r))
 -- (.)
 = fmap g . fmap h (Node l r)

