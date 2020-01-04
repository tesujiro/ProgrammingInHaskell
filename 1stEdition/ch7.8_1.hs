all' :: ( a -> Bool ) -> [a] -> Bool
all' p []  = True
all' p (x:xs) | p x       = all' p xs
              | otherwise = False

main = do
    print $ all' even [2,4,6,8]
    print $ all' even [2,4,6,7]
