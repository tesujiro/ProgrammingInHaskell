-- Definition
[] ++ ys = ys
(x:xs) ++ ys = x : (xs ++ ys)

-- Proposition 1
xs ++ [] = xs

-- Base
[] ++ []
 -- Definition
 = []

-- Recursion
if:
xs ++ [] = xs

then:
x:xs ++ []
 -- Definition
 = x : (xs ++ [])
 -- Assumption
 = x : xs


-- Proposition 2
xs ++ (ys ++ zs) = (xs ++ ys) ++ zs

-- Base
[] ++ (ys ++ zs)
 -- Definition
 = ys ++ zs
 -- Definition
 = ([] ++ ys) ++ zs

-- Recusion
if:
xs ++ (ys ++ zs) = (xs ++ ys) ++ zs

then:
(x:xs) ++ (ys ++ zs)
 -- Definition
 = x : (xs ++ (ys ++ zs))
 -- Assumption
 = x : ((xs ++ ys) ++zs)
 -- reverse Definition
 = (x:(xs ++ ys)) ++zs
 -- reverse Definition
 = ((x:xs) ++ ys) ++ zs

 
 
