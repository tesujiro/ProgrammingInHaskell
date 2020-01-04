all' :: ( a -> Bool ) -> [a] -> Bool
all' p []  = True
all' p (x:xs) | p x       = all' p xs
              | otherwise = False

any' :: ( a -> Bool ) -> [a] -> Bool
any' p []  = False
any' p (x:xs) | p x       = True
              | otherwise = any' p xs

takeWhile' :: ( a-> Bool ) -> [a] -> [a]
takeWhile' p [] = []
takeWhile' p (x:xs) | p x       = x:takeWhile' p xs
                    | otherwise = []

dropWhile' :: ( a-> Bool ) -> [a] -> [a]
dropWhile' p [] = []
dropWhile' p (x:xs) | p x       = dropWhile' p xs
                    | otherwise = x:xs

main = do
    print $ all' even [2,4,6,8]
    print $ all' even [2,4,6,7]
    print $ any' odd  [2,4,6,8]
    print $ any' even [2,4,6,8]
    print $ any' even [2,4,6,7]
    print $ takeWhile' even [2,4,6,7]
    print $ takeWhile' even [2,4,6,8]
    print $ takeWhile' even [1,2,4,6]
    print $ dropWhile' even [2,4,6,7]
    print $ dropWhile' even [2,4,6,8]
    print $ dropWhile' even [1,2,4,6]
