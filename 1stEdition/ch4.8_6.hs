mult :: Num a => a -> a -> a -> a
mult = \x -> (\y -> (\z -> x * y * z ))

main = do
    print $ mult 1 2 3
