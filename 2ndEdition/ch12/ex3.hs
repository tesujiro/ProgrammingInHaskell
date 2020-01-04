instance Applicative ((->) a) where
   -- pure :: b -> (a -> b)
   pure = const
   -- (<*>) :: (a -> b -> c) -> (a -> b) -> (a -> c)
   (f <*> g) x= f x (g x)
