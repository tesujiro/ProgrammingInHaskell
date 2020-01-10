-- Definition
fmap :: (a -> b) -> f a -> f b
fmap id      = id
fmap (g . h) = fmap g . fmap h

(.) :: (b -> c) -> (a -> b) -> (a -> c)
f . g = \x -> f (g x)

data Maybe a = Nothing | Just a
               deriving (Eq, Ord, Show, Read)

instance Functor Maybe where
-- fmap :: (a -> b) -> Maybe a -> Maybe b
fmap _ Nothing  = Nothing
fmap g (Just x) = Just (g x)

-- Proposition
fmap :: (a -> b) -> Maybe a -> Maybe b
fmap id = id
fmap (g . h) = fmap g . fmap h

-- fmap id x = id x
fmap id Nothing
 = Nothing
 = id Nothing
fmap id (Just x)
 = Just x
 = id (Just x)

-- fmap (g . h) = fmap g . fmap h 
fmap (g . h) Nothing
 = Nothing
 = (fmap g . fmap h) Nothing

fmap (g . h) (Just x)
 = Just ((g . h) x)
 -- Definitino of (.)
 = Just ((\x -> g (h x)) x)
 = Just (g (h x))
 -- fmap g (Just x) = Just (g x)   
 = fmap g (Just (h x))
 -- fmap g (Just x) = Just (g x)   
 = fmap g (fmap h (Just x))
 -- Definitino of (.)
 = (fmap g . fmap h) (Just x)
