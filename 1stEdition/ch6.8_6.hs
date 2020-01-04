sum2 :: Num a => [a] -> a
sum2 [] = 0
sum2 (x:xs) = x + sum2 xs

take2 :: Int -> [a] -> [a]
take2 0 xs = []
take2 n (x:xs) = x:take2 (n-1) xs

last2 :: [a] -> a
last2 [x]  = x
last2 (_:xs) = last2 xs

main = do
    print $ sum2 [1,2,3,4,5]
    print $ sum2 [1.2,3.45]
    print $ take2 3 [1,2,3,4,5]
    print $ last2 [1,2,3,4,5]
