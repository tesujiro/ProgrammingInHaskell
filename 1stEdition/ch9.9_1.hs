getLine' :: IO String
getLine' = do x <- getChar
              if x == '\n' then
                  return []
                else
                  do xs <- getLine'
                     return (x:xs)

--putStr :: String -> IO ()

readLine :: IO String
readLine = do x <- getChar
              case x of
                '\n'   -> return []
                '\DEL' -> do xs <- readLine
                             return ("\ESC[1D"++xs)
                _ -> do xs <- readLine
                        return (x:xs)

main = do
    --line <- getLine'
    line <- readLine
    putStrLn ("you said: " ++ line)
