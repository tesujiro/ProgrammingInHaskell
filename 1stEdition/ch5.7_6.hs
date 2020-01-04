find :: Eq a => a -> [(a,b)] -> [b]
find k t = [v| (k',v)<-t, k==k'] 

positions :: Eq a => a -> [a] -> [Int]
-- positions x xs = [i| (x',i) <- zip xs [1..n], x'==x] where n = length xs
positions x xs = find x (zip xs [1..(length xs)])

main = do
    print $ positions False [True, False, True, False]
