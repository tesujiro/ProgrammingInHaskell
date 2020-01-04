main = do
    print $ [(x,y) | x <- [1,2,3], y<- [4,5,6]]
    print $ concat[[(x,y)|y <- [4,5,6]]|x <- [1,2,3]]
