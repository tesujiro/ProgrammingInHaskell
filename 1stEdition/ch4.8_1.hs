halve :: [a] -> ([a],[a])
halve xs = (take l xs,drop l xs) where l = length xs `div` 2

main = do
    print $ halve [1,3,5,2,4,6]
    print $ halve [1,3]
    print $ halve [1]
