-- Definition
add :: Nat -> Nat -> Nat
add Zero m = m
add (Succ n) m = Succ (add n m)

-- Proposition
add n (Succ m) = Succ (add n m)

-- Base
add Zero (Succ m)
 -- add definition
 = Succ m
 -- reverse add definition
 = Succ (add Zero m)

-- Recursion
if:
 add n (Succ m) = Succ (add n m)
then:
add (Succ n) (Succ m)
 -- add definition
 = Succ (add n (Succ m))
 -- assumption
 = Succ (Succ (add n m))
 -- reverse add definition
 = Succ (add (Succ n) m)
