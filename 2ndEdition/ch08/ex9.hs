-- 8.7
data Expr = Val Int | Add Expr Expr | Mul Expr Expr

--value :: Expr -> Int
--value (Val n)   = n
--value (Add x y) = value x + value y

type Cont = [Op]
data Op = EVALa Expr | EVALm Expr | ADD Int | MUL Int

eval :: Expr -> Cont -> Int
eval (Val n)   c = exec c n
eval (Add x y) c = eval x (EVALa y:c)
eval (Mul x y) c = eval x (EVALm y:c)

exec :: Cont -> Int -> Int
exec [] n = n
exec (EVALa y:c) n = eval y (ADD n:c)
exec (EVALm y:c) n = eval y (MUL n:c)
exec (ADD n:c)  m = exec c (n + m)
exec (MUL n:c)  m = exec c (n * m)

value :: Expr -> Int
value e = eval e []

main = do
    print $ value (Add (Add (Val 2) (Val 3)) (Val 4))
    print $ value (Mul (Add (Val 2) (Val 3)) (Val 4))
