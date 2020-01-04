newtype ZipList a = Z [a] deriving Show

instance Functor ZipList where
   -- fmap :: (a -> b) -> ZipList a -> ZipList b
   fmap g (Z xs) = Z [g x | x <- xs]

instance Applicative ZipList where
   -- pure :: a -> ZipList a
   pure x = Z (repeat x)

   -- (<*>) :: ZipList (a -> b) -> ZipList a -> ZipList b
   (Z gs) <*> (Z xs) = Z [g x | (g,x) <- zip gs xs]

main = do
    print $ pure (+1) <*> (Z [1,2,3])
    print $ pure (+)  <*> (Z [1,2,3]) <*> (Z [4,5,6])
    print $ pure (\x y z -> x + y + z)  <*> (Z [1,2,3]) <*> (Z [4,5,6]) <*> (Z [100,100,100])
    print $ pure (&&)  <*> (Z [True,False,True]) <*> (Z [True,True,False])
