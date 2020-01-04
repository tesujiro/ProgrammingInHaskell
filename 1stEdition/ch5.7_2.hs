replicate2 :: Int -> a -> [a]
replicate2 n x = [x| _ <- [1..n]]

main = do
    print $ replicate2 3 True
