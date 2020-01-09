
-- Definition
(++) :: [a] -> [a] -> [a]
[] ++ ys     = ys
(x:xs) ++ ys = x : (xs ++ ys)

take 0 _  = []
take _ [] = []
take n (x:xs) = x : take (n-1) xs

drop 0 xs = xs
drop _ [] = []
drop n (_:xs) = drop (n-1) xs

-- Proposiiton
take n xs ++ drop n xs = xs
where n >= 0


-- Base 1
take 0 xs ++ drop 0 xs
 -- Definition
 = [] ++ drop 0 xs
 -- Definition
 = [] ++ xs
 -- Definition
 = xs

-- Base 2
take n [] ++ drop n []
 -- Definition
 = [] ++ []
 -- Definition
 = []

-- Recursion
if:
take n xs ++ drop n xs = xs

then:
take (n+1) (x:xs) ++ drop (n+1) (x:xs)
 -- Definition of take
 = (x : take n xs) ++ drop n xs
 -- Definition of ++
 = x : (take n xs ++ drop n xs)
 -- Assumption
 = x : xs




