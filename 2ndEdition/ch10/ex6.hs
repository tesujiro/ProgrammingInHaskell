import System.IO

getCh :: IO Char
getCh = do hSetEcho stdin False
           x <- getChar
           hSetEcho stdin True
           return x

readLine :: IO String
readLine = do
        x <- getCh
        case x of
          '\n' -> do
             return []
          '\\' -> do
             xs <- readLine
             return ('\b':xs)
          _ -> do
             xs <- readLine
             return (x:xs)

main = do
    putStrLn "Input String?"
    line <- readLine
    putStrLn line
    --print $ (show line)
