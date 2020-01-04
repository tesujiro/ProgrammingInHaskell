instance Monad ((->) a) where
   -- return :: b -> a -> b
   return = const

   -- (>>=) :: (a -> b) -> (b -> a -> c) -> a -> c
   (>>=) f g x = g (f x) x
