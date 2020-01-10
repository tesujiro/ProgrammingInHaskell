-- Definition
data Maybe a = Nothing | Just a
               deriving (Eq, Ord, Show, Read)

(.) :: (b -> c) -> (a -> b) -> (a -> c)
f . g = \x -> f (g x)

instance Functor Maybe where
-- fmap :: (a -> b) -> Maybe a -> Maybe b
fmap _ Nothing  = Nothing
fmap g (Just x) = Just (g x)

instance Applicative Maybe where
   -- pure :: a -> Maybe a
   pure = Just
   -- (<*>) :: Maybe (a -> b) -> Maybe a -> Maybe b
   Nothing <*> _ = Nothing
   (Just g) <*> mx = fmap g mx

pure :: a -> f a
(<*>) :: f (a -> b) -> f a -> f b

-- Proposition 1
pure id <*> x = x
-- Base
pure id <*> Nothing
 -- Definition of Applicative
 = Just id <*> Nothing
 -- Definition of Applicative
 = fmap id Nothing 
 -- Definition of Functor
 = Nothing

pure id <*> Just x
 -- Definition of Applicative
 = (Just id) <*> Just x
 -- Definition of Applicative
 = fmap id (Just x)
 -- Definition of Functor
 = Just (id x)
 -- id
 = Just x

-- Proposition 2
pure (g x)    = pure g <*> pure x
-- Base
pure g <*> pure x
 -- Definition of Applicative
 = (Just g) <*> (Just x)
 -- Definition of Applicative
 = fmap g (Just x)
 -- Definition of Functor
 = Just (g x)
 -- Reverse Definition of Applicative
 = pure (g x)

-- Proposition 3
x <*> pure y    = pure (\g -> g y) <*> x
-- Base
pure (\g -> g y) <*> Nothing
 -- Definition of Applicative
 = (Just (\g -> g y)) <*> Nothing 
 -- Definition of Applicative
 = fmap (\g -> g y) Nothing
 -- Definition of Functor
 = Nothing
 -- Reverse Definition of Applicative
 = Nothing <*> pure y
pure (\g -> g y) <*> Just x
 -- Definition of Applicative
 = (Just (\g -> g y)) <*> Just x 
 -- Definition of Applicative
 = fmap (\g -> g y) (Just x)
 -- Definition of Functor
 = Just ((\g -> g y) (Just x))
 = Just ((Just x) y) 
 -- *****************

 = Just (x y)
 -- Definition of Functor
 -- fmap g (Just x) = Just (g x)
 = fmap x (Just y)
 -- Definition of Applicative
 --  (Just g) <*> mx = fmap g mx
 = (Just x) <*> Just y
 -- Definition of Applicative
 = (Just x) <*> pure y

-- Proposition 4
x <*> (y <*> z) = (pure (.) <*> x <*> y) <*> z
-- Nothing 1
(pure (.) <*> Nothing <*> y) <*> z
 = ((pure (.) <*> Nothing) <*> y) <*> z
 -- Definition of Applicative
 = ((Just (.) <*> Nothing) <*> y) <*> z
 -- Definition of Applicative
 = ((fmap (.) Nothing) <*> y) <*> z
 -- Definition of Functor
 = (Nothing <*> y) <*> z
 -- Definition of Applicative
 = Nothing <*> z
 -- Definition of Applicative
 = Nothing
 -- Reverse Definition of Applicative
 = Nothing <*> (y <*> z)
-- Nothing 2
(pure (.) <*> x <*> Nothing) <*> z
 -- OMIT
 = x <*> (Nothing <*> z)
-- Just
(pure (.) <*> Just x <*> Just y) <*> z
 = ((pure (.) <*> Just x) <*> Just y) <*> z
 -- Definition of Applicative
 = ((Just (.) <*> Just x) <*> Just y) <*> z
 -- Definition of Applicative
 = ((fmap (.) (Just x)) <*> Just y) <*> z
 -- Definition of Functor
 = ((Just ((.) x)) <*> Just y) <*> z
 -- Definition of Applicative
 = (fmap ((.) x) (Just y)) <*> z
 -- Definition of Functor
 = (Just (((.) x) y)) <*> z
 -- (.)
 = (Just (x . y)) <*> z
 -- Definition of Applicative
 = fmap (x . y) (Just z)
 -- Definition of Functor
 = Just ((x . y) z)
 -- (.)
 = Just (x (y z))
 -- Definition of Applicative
 = fmap x (Just (y z))
 -- Definition of Functor
 = fmap x (fmap y (Just z))
 -- Definition of Applicative
 = fmap x ((Just y) <*> z)
 -- Definition of Applicative
 = (Just x) <*> (Just y <*> z)

