import System.IO

adder :: IO ()
adder = do putStr "How many numbers? " >> hFlush stdout
           str <- getLine
           let n = (read str)::Int
           t <- adder' 0 n
           putStrLn ("The total is " ++ show(t))

adder' :: Int -> Int -> IO Int
adder' t 0 = return t
adder' t n = do str <- getLine
                let x = (read str)::Int
                adder' (t + x) (n-1)

main = do
    --hSetBuffering stdout NoBuffering
    adder
