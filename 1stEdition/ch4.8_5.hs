(^^^) :: Bool -> Bool -> Bool

x ^^^ y = if x then y else False

main = do
    print $ True  ^^^ True
    print $ True  ^^^ False
    print $ False ^^^ True
    print $ False ^^^ False
