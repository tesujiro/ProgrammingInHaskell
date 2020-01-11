-- Functor
instance Functor [] where
   -- fmap :: (a -> b) -> f a -> f b
   fmap g [] = []
   fmap g (x:xs) = fmap g xs ++ [g x]

-- Applicative
instance Applicative [] where
   -- pure :: a -> [a]
   pure x = [x]
   -- (<*>) :: [a -> b] -> [a] -> [b]
   gs <*> xs = [g x | g <- gs, x <- xs]

-- Monad
instance Monad [] where
   -- (>>=) :: [a] -> (a -> [b]) -> [b]
   xs >>= f = [y | x <- xs, y <- f x]
   -- return :: a -> [a]
   return = pure

-- Definition



-- Monad laws
return x   >>= f      = f x
mx         >>= return = mx
(mx >>= f) >>= g      = mx >>= (\x -> (f x >>= g))

-- Proposition 1
return x >>= f = f x
-- Proof
return x >>= f
 -- Apply definition of monad []
 = [y' | x' <- return x, y' <- f x']
 -- Apply definition of monad []
 = [y' | x' <- pure x, y' <- f x']
 -- Apply definition of applicative []
 = [y' | x' <- [x], y' <- f x']
 = [y' | y' <- f x]
 = [y | y <- f x]
 = f x

-- Proposition 2
mx >>= return = mx
-- Proof
 mx >>= return
 -- Apply definition of monad []
 = [y | x <- mx, y <- return x] 
 -- Apply definition of monad []
 = [y | x <- mx, y <- pure x]
 -- Apply definition of applicative []
 = [y | x <- mx, y <- [x]]
 = mx 

-- Proposition 3
(mx >>= f) >>= g      = mx >>= (\x -> (f x >>= g))
-- Proof 1
(mx >>= f) >>= g
 -- Apply definition of monad []
 = [y | x <- mx, y <- f x] >>= g
 -- Apply definition of monad []
 = [y' | x' <- [y | x <- mx, y <- f x], y' <- g x']
 = [y' | x <- mx, y <- f x, y' <- g y]
 = [y' | x <- mx, y' <- (g . f) x]
 = [y | x <- mx, y <- (g . f) x]
-- Proof 2
mx >>= (\x -> (f x >>= g))
 -- Apply definition of monad []
 = mx >>= (\x -> [y' | x' <- f x, y' <- g x'])
 = mx >>= (\x -> [x' | x' <- (g . f) x])
 -- Apply definition of monad []
 = [y'' | x'' <- mx, y'' <- (\x -> [x' | x' <- (g . f) x]) x'']
 = [y'' | x'' <- mx, y'' <- [x' | x' <- (g . f) x'']]
 = [y | x <- mx, y <- [x' | x' <- (g . f) x]]
 = [y | x <- mx, y <- (g . f) x]
