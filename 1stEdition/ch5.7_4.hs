perfects :: Int -> [Int]
perfects n = [x | x<- [1..n], sum[y| y <- [1..x], x `mod` y == 0, y /= x ] == x]

main = do
    print $ perfects 10000
