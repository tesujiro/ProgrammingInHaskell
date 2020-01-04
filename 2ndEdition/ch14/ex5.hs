

filterF :: Foldable t => (a -> Bool) -> t a -> [a]
filterF p = foldMap (\x -> if p x then [x] else mempty) 

main = do
     print $ filterF (> 5) [1,5,10,15]
     print $ filterF (/= 0) (Just 5)
