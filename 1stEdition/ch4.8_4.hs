(^^^) :: Bool -> Bool -> Bool

x ^^^ y = if x then if y then True else False else False

main = do
    print $ True  ^^^ True
    print $ True  ^^^ False
    print $ False ^^^ True
    print $ False ^^^ False
