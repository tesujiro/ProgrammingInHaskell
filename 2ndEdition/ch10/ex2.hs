type Board = [Int]

initial :: Board
initial = [5,4,3,2,1]

putRow :: Int -> Int -> IO ()
putRow row num = do putStr (show row)
                    putStr ": "
                    putStrLn (concat (replicate num "* "))

putBoard :: Board -> IO ()
putBoard [a,b,c,d,e] = do putRow 1 a
                          putRow 2 b
                          putRow 3 c
                          putRow 4 d
                          putRow 5 e

putBoardHelper :: Int -> Board -> IO ()
putBoardHelper _ [] = return ()
putBoardHelper n (x:xs) = do putRow n x
                             putBoardHelper (n+1) xs

putBoard' :: Board -> IO ()
putBoard' = putBoardHelper 1

main = do
    putBoard initial
    putBoard' [1,2,3,4,3,2,1]
