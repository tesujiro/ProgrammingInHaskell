curry' :: ( (a, b) -> c ) -> ( a -> b -> c )
curry' f a b = f (a, b)

uncurry' :: ( a -> b-> c ) -> ( (a, b) -> c )
uncurry' f (a, b) = f a b

add :: Num a => (a, a) -> a
add (x, y) = x + y

add' :: Num a => a -> a -> a
add' x y = x + y

main = do
    print $ curry' add 1 2
    print $ uncurry' add' (1, 2)
    print $ uncurry' (+) (1, 2)
