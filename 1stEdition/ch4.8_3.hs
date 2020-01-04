(|||) :: Bool -> Bool -> Bool

True  ||| _ = True
False ||| b = b

main = do
    print $ True  ||| True
    print $ True  ||| False
    print $ False ||| True
    print $ False ||| False
