-- 9.2
data Op = Add | Sub | Mul | Div

instance Show Op where
   show Add = "+"
   show Sub = "-"
   show Mul = "*"
   show Div = "/"

valid :: Op -> Int -> Int -> Bool
valid Add _ _ = True
valid Sub x y = x > y
valid Mul _ _ = True
valid Div x y = x `mod` y == 0

apply :: Op -> Int -> Int -> Int
apply Add x y = x + y
apply Sub x y = x - y
apply Mul x y = x * y
apply Div x y = x `div` y

-- 9.3
data Expr = Val Int | App Op Expr Expr

instance Show Expr where
   show (Val n)  = show n
   show (App o l r)= brak l ++ show o++ brak r
                    where
                      brak (Val n) = show n
                      brak e = "(" ++ show e ++ ")"

values :: Expr -> [Int]
values (Val n) = [n]
values (App _ l r) = values l ++ values r

eval :: Expr -> [Int]
eval (Val n)     = [n | n > 0]
eval (App o l r) = [apply o x y | x <- eval l,
                                  y <- eval r,
                                  valid o x y]
-- 9.4
subs :: [a] -> [[a]]
subs [] = [[]]
subs (x:xs) = yss ++ map (x:) yss
              where yss = subs xs

interleave :: a -> [a] -> [[a]]
interleave x [] = [[x]]
interleave x (y:ys) = (x:y:ys) : map (y:) (interleave x ys)

perms :: [a] -> [[a]]
perms [] = [[]]
perms (x:xs) = concat (map (interleave x) (perms xs))

choices :: [a] -> [[a]]
choices = concat . map perms . subs

-- 9.5
solution :: Expr -> [Int] -> Int -> Bool
solution e ns n =
   elem (values e) (choices ns) && eval e == [n]

-- excercise 2
rmFirstOne :: Eq a => a -> [a] -> [a]
rmFirstOne _ [] = []
rmFirstOne x (y:ys) | x == y    = ys
                    | otherwise = y : (rmFirstOne x ys)

isChoice :: Eq a => [a] -> [a] -> Bool
isChoice [] _ = True
isChoice _ [] = False
isChoice (x:xs) ys = elem x ys && isChoice xs (rmFirstOne x ys)

main = do
    print $ show (App Add (Val 1) (App Mul (Val 2) (Val 3)))
    print $ eval (App Add (Val 2) (Val 3))
    print $ eval (App Sub (Val 2) (Val 3))
    print $ perms [1,2,3]
    print $ choices [1,2,3]
    let e = App Mul (App Add (Val 1) (Val 50)) (App Sub (Val 25) (Val 10))
    print $ solution e [1,3,7,10,25,50] 765
    print $ solution e [  3,7,10,25,50] 765
    print $ rmFirstOne 3 [1,2,3,4,5]
    print $ rmFirstOne 3 [1,2,3,4,3]
    print $ isChoice [1,2,3] [1,2,3,4,5]
    print $ isChoice [3,1,2] [1,2,3,4,5]
    print $ isChoice [1,2,3] [1,2,4,5]
    print $ isChoice [2,2,3] [1,2,3,4,2]
    print $ isChoice [2,2,3,3] [1,2,3,4,2]


