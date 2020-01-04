putStr' :: String -> IO ()
putStr' xs=sequence_ [putChar x | x<-xs]

main = do
    putStr "aaa"
    putStr' "bbb"
