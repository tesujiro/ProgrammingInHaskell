class Eq' a where
  (===), (/==) :: a -> a -> Bool

  x /== y = not (x === y)

instance Eq a => Eq' (Maybe a) where
   (Just x) === (Just y) = x==y
   _ === _               = False


instance Eq a => Eq' [a] where
  []   === []   = True
  (x:xs) === (y:ys) = x==y && xs === ys
  _    === _    = False

main = do
    print $ (Just True) === (Just True)
    print $ (Just False) === (Just True)
    print $ Nothing === (Just True)
    print $ [1,2,3] === [1,2,3]
    print $ [1,2,3] === [1,2,5]
    print $ [1,3] === [1,2,5]
