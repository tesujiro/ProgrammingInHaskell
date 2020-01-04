merge :: Ord a => [a] -> [a] -> [a]
merge xs [] = xs
merge [] ys = ys
merge (x:xs) (y:ys) | x < y     = x: merge xs (y:ys)
                    | otherwise = y : merge (x:xs) ys

main = do
    print $ merge [1,2,5,8] [3,4,7,9]
