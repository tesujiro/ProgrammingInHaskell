scalarproduct :: [Int] -> [Int] -> Int
scalarproduct xs ys = sum[ x * y | (x,y) <- zip xs ys]

main = do
    print $ scalarproduct [1,2,3] [4,5,6]
