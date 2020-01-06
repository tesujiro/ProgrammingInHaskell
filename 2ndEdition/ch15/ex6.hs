initValue=1.0
minDelta=0.00001

sqRootList :: Double -> [Double]
sqRootList  n = iterate (\a -> (a + n/a ) / 2) initValue

sqRoot :: Double -> Double
sqRoot n = head [y | (x,y) <- zip sqRootList (tail sqRootList), abs (x - y) < minDelta]
           where sqRootList = iterate (\a -> (a + n/a ) / 2) initValue

main = do
    print $ take 10 (sqRootList 5.0)
    print $ sqRoot 5.0
