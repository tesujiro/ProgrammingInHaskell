data Nat = Zero | Succ Nat

nat2int :: Nat -> Int
nat2int Zero = 0
nat2int (Succ n) = 1 + nat2int n

add :: Nat -> Nat -> Nat
add Zero     n = n
add (Succ m) n = Succ (add m n)

mult :: Nat -> Nat -> Nat
mult Zero    n = Zero
mult (Succ m) n = add n (mult m n)

main = do
    print $ nat2int(add Zero Zero)
    print $ nat2int(Succ Zero)
    print $ nat2int(add (Succ (Succ Zero)) (Succ (Succ Zero))) -- 2+2
    print $ nat2int(mult (Succ (Succ Zero)) Zero) -- 2*0
    print $ nat2int(mult (Succ (Succ Zero)) (Succ (Succ (Succ Zero))))  -- 2*3

