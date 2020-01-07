-- Definition
add :: Nat -> Nat -> Nat
add Zero m = m
add (Succ n) m = Succ (add n m)

-- Proposition
add n m = add m n

-- Base
add Zero m
 -- add definition
 = m
 -- reverse add definition
 = add m Zero

-- Recurtion
if:
 add n m = add m n
then:
 add (Succ n) m
 -- add definition
 = Succ (add n m)
 -- assumption
 = Succ (add m n)
 -- ex.1
 = add m (Succ n)


