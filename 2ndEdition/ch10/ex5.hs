import System.IO

adder :: IO ()
adder = do putStr "How many numbers? " >> hFlush stdout
           str <- getLine
           let n = (read str)::Int
           ns <- sequence (replicate n inNum)
           putStrLn ("The total is " ++ show(sum ns))

inNum :: IO Int
inNum = do str <- getLine
           return ((read str)::Int)

main = do
    --hSetBuffering stdout NoBuffering
    adder
