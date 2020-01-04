import Data.Char

type Parser a = String -> [(a, String)]

return' :: a -> Parser a
return' v = \inp -> [(v, inp)]

--failure :: Parser a -- Ambiguous type variable
failure :: Parser Char
failure = \inp -> []

item :: Parser Char
item = \inp -> case inp of
                 []      -> []
                 (x: xs) -> [(x, xs)]

parse   :: Parser a -> String -> [(a, String)]
parse p inp = p inp

sat :: ( Char -> Bool) -> Parser Char
sat p = do x <- item
           if p x then return x else failure

digit :: Parser Char
digit = sat isDigit

-------------------------
-- ex.1

--int :: Parser Int
--p = do 

main = do
    print $ parse (return' 1) "abc"
    print $ parse failure "abc" 
    print $ parse item ""
    print $ parse item "abc"
    --print $ parse digit "123"
