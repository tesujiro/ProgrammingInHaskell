and2 :: [Bool] -> Bool
and2 [b] = b
and2 (True:xs) = and2 xs
and2 (False:xs) = False

concat2 :: [[a]] -> [a]
concat2 []=[]
concat2([]:ys)=concat2 ys
concat2((x:xs):ys)=x:concat2(xs:ys)

replicate2 :: Int -> a -> [a]
replicate2 0 x = []
replicate2 n x = x:replicate2 (n-1) x

(!!!) :: [a] -> Int -> a
(x:_)  !!! 1 = x
(x:xs) !!! n = xs !!! (n-1)

elem2 :: Eq a => a -> [a] -> Bool
elem2 x [] = False
elem2 x (y:ys) | x == y    = True
               | otherwise = elem2 x ys

main = do
    print $ and2 [True,False]
    print $ and2 [True,True,True]
    print $ and2 [True,True,False]
    print $ and2 [False,True,True]
    print $ concat2 [[1,2,3],[4,5,6]]
    print $ concat2 [[],[4,5,6]]
    print $ concat2 [[1,2,3],[]]
    print $ replicate2 3 1
    print $ replicate2 5 'a'
    print $ [1,2,3,4,5] !!! 3
    print $ elem2 3 [1,2,3,4,5]
    print $ elem2 100 [1,2,3]
