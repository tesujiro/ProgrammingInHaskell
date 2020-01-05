import           System.Environment (getArgs)

fibs :: [Integer]
fibs = 0:1:[x+y | (x,y) <- zip fibs (tail fibs)]

main = do
     args <- getArgs
     let n = if length args > 0 then read (args !! 0)::Int else 50
     print $ take n fibs
