-- 7.7

rmdups :: Eq a => [a] -> [a]
rmdups [] = []
rmdups (x:xs) = x : rmdups (filter (/= x) xs)

-- 8.1

type Assoc k v = [(k, v)]

find :: Eq k => k -> Assoc k v -> v
find k t = head [v | (k', v) <- t, k == k']

-- 8.6

data Prop = Const Bool
          | Var Char
          | Not Prop
          | And Prop Prop
          | Or  Prop Prop
          | Imply Prop Prop
          | Equal Prop Prop

type Subst = Assoc Char Bool

eval :: Subst -> Prop -> Bool
eval _ (Const b) = b
eval s (Var x) = find x s
eval s (Not p) = not (eval s p)
eval s (And p q) = eval s p && eval s q
eval s (Or  p q) = eval s p || eval s q
eval s (Imply p q) = eval s p <= eval s q
eval s (Equal p q) = eval s p == eval s q

vars :: Prop -> [Char]
vars (Const _) = []
vars (Var x) = [x]
vars (Not p) = vars p
vars (And p q) = vars p ++ vars q
vars (Or  p q) = vars p ++ vars q
vars (Imply p q) = vars p ++ vars q
vars (Equal p q) = vars p ++ vars q

bools :: Int -> [[Bool]]
bools 0 = [[]]
bools n = map (False:) bss ++ map (True:) bss
          where bss = bools (n-1)

substs :: Prop -> [Subst]
substs p = map (zip vs) (bools (length vs))
           where vs = rmdups (vars p)

isTaut :: Prop -> Bool
isTaut p = and [eval s p | s <- substs p]

main = do
    print $ isTaut (Const True)
    print $ isTaut (And (Var 'A') (Not (Var 'A')))
    print $ isTaut (Or  (Var 'A') (Not (Var 'A')))
    print $ isTaut (Equal  (Var 'A') (Var 'A'))
    print $ isTaut (Equal  (Var 'A') (Not (Var 'A')))

