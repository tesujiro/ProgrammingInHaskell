import Data.Foldable

instance Foldable Maybe where
   -- fold :: Monoid a => Maybe a -> a
   fold Nothing = Nothing
   fold Just x  = x

   -- foldMap :: Monoid b => (a -> b) -> Maybe a -> b
   fold _ Nothing  = Nothing
   fold f (Just x) = f x

   -- foldr :: (a -> b -> b) -> b -> Maybe b -> b
   foldr _ z Nothing  = z
   foldr f z (Just x) = f x z
   
   -- foldl :: (b -> a -> b) -> b -> Maybe b -> b
   foldl _ z Nothing  = z
   foldl f z (Just x) = f z x


instance Traversable Maybe where
   -- traverse :: Applicative f => (a -> f b) -> Maybe a -> f (Maybe b)
   traverse g Nothing = Nothing
   --traverse g (Just x) = pure Just <*> g x
   traverse g (Just x) = Just <$> g x

