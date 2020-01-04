safetail1 :: [a] -> [a]
safetail1 xs = if null xs then [] else tail xs

safetail2 :: [a] -> [a]
safetail2 xs | null xs   = [] 
             | otherwise = tail xs

safetail3 :: [a] -> [a]
safetail3 [] = []
safetail3 xs = tail xs

main = do
    print $ safetail1 [1,2,3]
    print $ safetail1 [1]
    print $ safetail1 ([]::[Int])
    print $ safetail2 [1,2,3]
    print $ safetail2 [1]
    print $ safetail2 ([]::[Int])
    print $ safetail3 [1,2,3]
    print $ safetail3 [1]
    print $ safetail3 ([]::[Int])

