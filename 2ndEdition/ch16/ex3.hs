
-- Definition
all p [] = True
all p (x:xs) = p x && all p xs

replicate :: Int -> a -> [a]
replicate 0 _ = []
replicate n x = x : replicate (n-1) x

-- Proposition
all (== x) (replicate n x)

-- Base
all (== x) (replicate 0 x)
 -- Defiition of replicate
 = all (== x) []
 -- Definition of all
 = True

-- Recursion
if:

all (== x) (replicate n x) = True

then:

all (== x) (replicate (n+1) x) = True
 -- Defiition of replicate
 = all (== x) x : (replicate n x)
 -- Definition of all
 = (==x) x && all (== x) (replicate n x)
 = True && all (== x) (replicate n x)
 = all (== x) (replicate n x)
 -- assumption
 = True

