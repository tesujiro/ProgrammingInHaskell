-- Chapter 13.3

import Control.Applicative
import Data.Char

newtype Parser a = P (String -> [(a,String)])

parse :: Parser a -> String -> [(a,String)]
parse (P p) inp = p inp

item :: Parser Char
item = P (\inp -> case inp of
                     [] -> []
                     (x:xs) -> [(x,xs)])

-- Chapter 13.4

instance Functor Parser where
   -- fmap :: (a -> b) -> Parser a -> Parser b
   fmap g p = P (\inp -> case parse p inp of
                            [] -> []
                            [(v,out)] -> [(g v, out)])

instance Applicative Parser where
   -- pure :: a -> Parser a
   pure v = P (\inp -> [(v,inp)])

   -- (<*>) :: Parser (a -> b) -> Parser a -> Parser b
   pg <*> px = P (\inp -> case parse pg inp of
                             [] -> []
                             [(g,out)] -> parse (fmap g px) out)

instance Monad Parser where
   -- (>>=) :: Parser a -> (a -> Parser b) -> Parser b
   p >>= f = P (\inp -> case parse p inp of
                           [] -> []
                           [(v,out)] -> parse (f v) out)

-- Chapter 13.5

instance Alternative Parser where
   -- empty :: f a
   -- empty :: Parser a
   empty = P (\inp -> [])

   -- (<|>) :: f a -> f a -> f a
   -- (<|>) :: Parser a -> Parser a -> Parser a
   p <|> q = P (\inp -> case parse p inp of
                           [] -> parse q inp
                           [(v,out)] -> [(v,out)])

-- Chapter 13.6
sat :: (Char -> Bool) -> Parser Char
sat p = do x <- item
           if p x then return x else empty

digit :: Parser Char
digit = sat isDigit

lower :: Parser Char
lower = sat isLower

upper :: Parser Char
upper = sat isUpper

letter :: Parser Char
letter = sat isAlpha

alphanum :: Parser Char
alphanum = sat isAlphaNum

char :: Char -> Parser Char
char x = sat (== x)

string :: String -> Parser String
string [] = return []
string (x:xs) = do char x
                   string xs
                   return (x:xs)

ident :: Parser String
ident = do x <- lower
           xs <- many alphanum
           return (x:xs)

nat :: Parser Int
nat = do xs <- some digit
         return (read xs)

space :: Parser ()
space = do many (sat isSpace)
           return ()

int :: Parser Int
int = do char '-'
         n <- nat
         return (-n)
      <|> nat

-- excercise 1
comment :: Parser ()
comment = do char '-'
             char '-'
             many (sat (/= '\n'))
             char '\n'
             return ()

-- Chapter 13.7
-- Chapter 13.8
-- Chapter 13.9

main = do
    print $ (parse empty "abc"::[(Char,String)])
    print $ parse (item <|> return 'd') "abc"
    print $ parse digit "123"
    print $ parse digit "abc"
    print $ parse (char 'a') "abc"
    print $ parse (string "abc") "abcdef"
    print $ parse (many digit) "123abc"
    print $ parse (many digit) "abcdef"
    print $ parse (some digit) "abcdef"
    print $ parse ident "abc def"
    print $ parse nat "123XYZ"
    print $ parse space "   XYZ"
    print $ parse int "123XYZ"
    print $ parse int "-123XYZ"
    print $ parse comment "--123XYZ\n"
    print $ parse comment "-123XYZ\n"
    print $ parse comment "--123XYZ"
