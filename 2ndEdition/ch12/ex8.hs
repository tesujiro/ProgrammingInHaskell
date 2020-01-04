type State = Int

newtype ST a = S (State -> (a,State))

app :: ST a -> State -> (a,State)
app (S st) x = st x

instance Functor ST where
   -- fmap :: (a -> b) -> ST a -> ST b
   -- fmap g st = S (\s -> let (x,s') = app st s in (g x, s'))
   fmap g st = do
            x <- st
            return (g x)

instance Applicative ST where
   -- pure :: a -> ST a
   pure x = S (\s -> (x,s))
   -- (<*>) :: ST (a -> b) -> ST a -> ST b
   -- stf <*> stx = S (\s ->
      -- let (f,s') = app stf s
          -- (x,s'') = app stx s' in (f x, s''))
   stf <*> stx = do
              f <- stf
              x <- stx
              return (f x)

instance Monad ST where
   -- (>>=) :: ST a -> (a -> ST b) -> ST b
   st >>= f = S (\s ->
      let (x,s') = app st s in app (f x) s')

main = do
    let state1 = 1 :: State
    let state2 = 2 :: State
    let state100 = 100 :: State
    let st1 = S (\x -> if x == state1 then (10,state2) else (20,state1))
    print $ state1
    print $ app st1 state1
    print $ app st1 state2

    putStrLn $ id "== Functor"
    let st2 = fmap  (+1000) st1
    print $ app st2 state1
    print $ app st2 state2

    putStrLn $ id "== Applicative"
    let st100 = pure 100
    print $ app st100 state1
    let st100' = pure id <*> st100
    print $ app st100' state2

    putStrLn $ id "== Monad"
    let add12 = do
             x <- st1
             y <- st2
             return (x + y)
    print $ app add12 state1
    print $ app add12 state2
    print $ app add12 state100
    let mul100 = do
             x <- st1
             return (x * 100)
    print $ app mul100 state1
    print $ app mul100 state2
    print $ app mul100 state100

    
