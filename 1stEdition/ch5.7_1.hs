sqSum :: Int -> Int
sqSum n = sum[ x^2 | x <- [1 .. n]]

main = do
    print $ sqSum 3
