data Expr = Val Int | Add Expr Expr

folde :: (Int -> a) -> (a -> a -> a) -> Expr -> a
folde f g (Val x) = f x
folde f g (Add l r) = g (folde f g l) (folde f g r)

eval :: Expr -> Int
eval = folde (\x -> x) (\x y -> x + y)

main = do
    print $ eval (Add (Val 1) (Val 2))
