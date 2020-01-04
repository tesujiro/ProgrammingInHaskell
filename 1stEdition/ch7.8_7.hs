unfold :: (a -> Bool) -> (a -> b) -> (a -> a) -> a -> [b]
unfold p h t x | p x       = []
               | otherwise = h x : unfold p h t (t x)

type Bit = Int

chop8 :: [Bit] -> [[Bit]]
chop8 [] = []
chop8 bits = take 8 bits: chop8 (drop 8 bits)

chop8' :: [Bit] -> [[Bit]]
chop8' = unfold null (take 8) (drop 8)

map' :: (a -> b) -> [a] -> [b]
map' f = unfold null (f.head) tail

iterate' :: (a -> a) -> a -> [a]
iterate' f = unfold (\_ -> False) f f

main = do
    print $ chop8  [0, 0, 0, 1, 0, 0, 1, 0, 0, 0, 1, 1, 0, 1, 0, 0]
    print $ chop8' [0, 0, 0, 1, 0, 0, 1, 0, 0, 0, 1, 1, 0, 1, 0, 0]
    print $ map' (*2) [1, 2, 3]
    print $ take 5 (iterate' (*2) 1)
    print $ take 5 (iterate' id 'a')
