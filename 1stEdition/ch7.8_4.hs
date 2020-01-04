dec2int :: [Int] -> Int
dec2int = foldl (\x -> \y -> 10*x+y ) 0

main = do
    print $ dec2int [1,2,3,4,5]
    print $ dec2int [3,2,1,0,1,2,3]
