pyths :: Int -> [(Int,Int,Int)]
pyths n = [(x,y,z)|x<-[1..n],y<-[x..n],z<-[1..n],x^2+y^2==z^2]

main = do
    print $ pyths 10
