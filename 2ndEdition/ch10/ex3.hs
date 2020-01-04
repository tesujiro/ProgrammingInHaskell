putStr' :: String -> IO ()
putStr' xs=sequence_ [putChar x | x<-xs]

type Board = [Int]

putRow :: Int -> Int -> IO ()
putRow row num = do putStr (show row)
                    putStr ": "
                    putStrLn (concat (replicate num "* "))

putBoard'' :: Board -> IO ()
putBoard'' xs = sequence_ [putRow r n | (r, n) <- zip [1..] xs]

main = do
    putBoard'' [1,2,3,4,3,2,1]
