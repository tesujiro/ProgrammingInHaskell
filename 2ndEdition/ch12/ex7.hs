import Data.Char

data Expr a = Var a | Val Int | Add (Expr a) (Expr a)
              deriving Show

instance Functor Expr where
   -- mapf :: (a -> b) -> f a -> f b
   -- mapf :: (a -> b) -> Expr a -> Expr b
   fmap f (Var x) = Var (f x)
   fmap _ (Val x) = Val x
   fmap f (Add l r) = Add (fmap f l) (fmap f r)

instance Applicative Expr where
   -- pure :: a -> f a
   -- pure :: a -> Expr a
   pure x = Var x
   -- <*> :: f (a -> b) -> f a -> f b
   -- <*>  :: Expr (a -> b) -> Expr a -> Expr b
   Var g <*> ex     = fmap g ex
   Val _ <*> Val x  = Val x
   Val x <*> _      = Val x
   Add f g <*> ex = Add (f <*> ex) (g <*> ex)

instance Monad Expr where
   -- return :: a -> m a
   -- return :: a -> Expr a
   return = pure
   -- (>>=) :: m a -> (a -> m b) -> m b
   -- (>>=) :: Expr a -> (a -> Expr b) -> Expr b
   Var x   >>= f   = f x
   Val x   >>= _   = Val x 
   Add ex ey >>= f = Add (ex >>= f) (ey >>= f)

main = do
    print $ Add (Var 'X') (Val 1)

    putStrLn $ id "== Functor"
    print $ fmap toUpper (Var 'a')
    print $ fmap (++"XYZ") (Var "ABC")
    print $ fmap (++[4,5,6]) (Var [1,2,3])
    print $ fmap (1+) (Val 2)
    print $ fmap (1+) (Add (Var 10) (Var 100))

    putStrLn $ id "== Applicative"
    print $ pure id <*> (Var 'z')
    print $ pure id <*> (Var (1+)) <*> (Val 10)
    print $ pure id <*> (Var (1+)) <*> (Var 10)
    print $ pure id <*> (Var (\x y -> x + y)) <*> (Val 10) <*> (Val 20)
    print $ pure id <*> (Var (\x y -> x + y)) <*> (Var 10) <*> (Var 20)
    print $ pure (\x y -> x + y) <*> (Var 10) <*> (Var 20)
    print $ pure id <*> (Add (Var (1+)) (Var (10+))) <*> (Var 100)
    print $ pure id <*> (Add (Var (1+)) (Var (10+))) <*> (Val 100)

    putStrLn $ id "== Monad"
    print $ Var 'X' >>= (\x ->return x)
    print $ Var 'X' >>= return
    print $ Var 'X' >>= \x ->
            Var 'Y' >>= \y ->
            Add (Var x) (Var y) >>= return
    let test2 = do
              z <- Add (Var 'x') (Var 'y') 
              return z
    print test2
    let add = (\x y -> do
            z <- Add x y
            return z)
    print $ add (Var 'X') (Var 'Y')
    let inc = (\x -> do
            z <- Add x (Val 1)
            return z)
    print $ inc (Var 'X') 
