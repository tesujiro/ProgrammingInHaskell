data Expr = Val Int | Add Expr Expr

folde :: (Int -> a) -> (a -> a -> a) -> Expr -> a
folde f g (Val x) = f x
folde f g (Add l r) = g (folde f g l) (folde f g r)

main = do
    print $ folde (\x -> 2 * x) (\x y -> x * y) (Add (Val 1) (Val 2))
