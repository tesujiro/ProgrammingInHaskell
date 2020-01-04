import Data.Monoid   -- use Sum
import Data.Foldable
import Data.Traversable

data Tree a = Leaf | Node (Tree a) a (Tree a)
              deriving Show

instance Foldable Tree where
   -- fold :: Monoid a => t a -> a
   -- fold :: Monoid a => Tree a -> a
   fold Leaf = mempty
   fold (Node l x r) = (fold l) `mappend` x `mappend` (fold r)

   -- foldMap :: Monoid b => (a -> b) -> t a -> b
   -- foldMap :: Monoid b => (a -> b) -> Tree a -> b
   foldMap f Leaf = mempty
   foldMap f (Node l x r) = (foldMap f l) `mappend` (f x) `mappend` (foldMap f r)

   -- foldr :: (a -> b -> b) -> b -> t a -> b
   -- foldr :: (a -> b -> b) -> b -> Tree a -> b
   foldr _ v Leaf = v
   foldr f v (Node l x r) = foldr f (f x (foldr f v r)) l

   -- foldl :: (a -> b -> a) -> a -> t b -> a
   -- foldl :: (a -> b -> a) -> a -> Tree b -> a
   foldl _ v Leaf = v
   foldl f v (Node l x r) = foldl f (f (foldl f v l) x) r

instance Functor Tree where
   -- fmap :: (a -> b) -> f a -> f b
   -- fmap :: (a -> b) -> Tree a -> Tree b
   fmap _ Leaf = Leaf
   fmap g (Node l x r) = Node (fmap g l) (g x) (fmap g r)

instance Traversable Tree where
   -- traverse :: Applicative f => (a -> f b) -> t a -> f (t b)
   -- traverse :: Applicative f => (a -> f b) -> Tree a -> f (Tree b)
   traverse g Leaf = pure Leaf
   traverse g (Node l x r) = pure Node <*> traverse g l <*> g x <*> traverse g r

main = do
    putStrLn $ id "== Foladable"
    print $ getSum (fold (Node (Node Leaf (Sum 100) Leaf) (Sum 10) Leaf))
    print $ getSum (foldMap (+1) (Node (Node Leaf (Sum 100) Leaf) (Sum 10) Leaf))
    print $ foldMap (\x -> ("x",x)) (Node (Node Leaf (Sum 100) Leaf) (Sum 10) Leaf)
    print $ getSum (foldr (-) 0 (Node (Node Leaf (Sum 100) Leaf) (Sum 10) Leaf)) -- 100-(10-0) = 90
    print $ getSum (foldl (-) 0 (Node (Node Leaf (Sum 100) Leaf) (Sum 10) Leaf)) -- (0-100)-10 = -110
    print $ getSum (foldr (-) 0 (Node (Node Leaf (Sum 100) Leaf) (Sum 10) (Node Leaf (Sum 1000) Leaf))) -- 100-(10-(1000-0)) = 1090
    print $ getSum (foldl (-) 0 (Node (Node Leaf (Sum 100) Leaf) (Sum 10) (Node Leaf (Sum 1000) Leaf))) -- ((0-100)-10)-1000 = -1110
    putStrLn $ id "== Traversable"
    print $ traverse (\x -> [x]) (Node Leaf 5 Leaf) 
    print $ traverse (\x -> [1..x]) (Node Leaf 5 Leaf) 
    print $ traverse (\x -> [1..x]) (Node Leaf 2 (Node Leaf 3 Leaf)) 
    print $ traverse (\x -> [(1,y) | y<-[1..x]]) (Node Leaf 2 (Node Leaf 3 Leaf)) 
    print $ traverse (\x -> Sum x) (Node (Node Leaf 100 Leaf) 10 Leaf)
    print $ traverse (\x -> (Sum x,x)) (Node (Node Leaf 100 Leaf) 10 Leaf)
    print $ traverse (\x -> ("x",x)) (Node (Node Leaf 100 Leaf) 10 Leaf)
    print $ traverse (\x -> (Sum x,"x")) (Node (Node Leaf 100 Leaf) 10 Leaf)


