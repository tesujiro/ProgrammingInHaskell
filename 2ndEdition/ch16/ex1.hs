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
 -- assumtion
 = Succ (Succ (add n m))
 -- reverse add definition
 = Succ (add (Succ n) m)
