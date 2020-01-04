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
token :: Parser a -> Parser a
token p = do space
             v <- p
             space
             return v

identifier :: Parser String
identifier = token ident

natural :: Parser Int
natural = token nat

integer :: Parser Int
integer = token int

symbol :: String -> Parser String
symbol xs = token (string xs)

nats :: Parser [Int]
nats = do symbol "["
          n <- natural
          ns <- many (do symbol ","
                         natural)
          symbol "]"
          return (n:ns)

-- Chapter 13.8
expr :: Parser Int
expr = do t <- term
          do symbol "+"
             e <- expr
             return (t + e)
           <|> return t

term :: Parser Int
term = do f <- factor
          do symbol "*"
             t <- term
             return (f * t)
           <|> return f

factor :: Parser Int
factor = do symbol "("
            e <- expr
            symbol ")"
            return e
          <|> integer

eval :: String -> Int
eval xs = case (parse expr xs) of
             [(n,[])]  -> n
             [(_,out)] -> error ("Unused input " ++ out)
             []        -> error "Invalid input"

-- Excercise 7
-- Changed
--   expr ::= term ( + expr | - expr | ϵ)
--   term ::= factor ( * term | / term | ϵ)
-- ==>
--   expr ::= term ( + term | - term )*
--   term ::= factor ( * factor | / factor )*
expr' :: Parser Int
expr' = do t1 <- term'
           rest t1
    where
      rest t1 =
           do symbol "+"
              t2 <- term' 
              rest (t1 + t2)
            <|> do symbol "-"
                   t2 <- term' 
                   rest (t1 - t2)
            <|> return t1

term' :: Parser Int
term' = do f1 <- factor'
           rest f1
    where
      rest f1 =
           do symbol "*"
              f2 <- factor'
              rest (f1 * f2)
            <|> do symbol "/"
                   f2 <- factor'
                   rest (f1 `div` f2)
            <|> return f1

factor' :: Parser Int
factor' = do sf1 <- subfactor'
             rest sf1
      where
        rest sf1 =
             do symbol "^"
                sf2 <- subfactor'
                rest (sf1 ^ sf2)
              <|> return sf1

subfactor' :: Parser Int
subfactor' = do symbol "("
                e <- expr'
                symbol ")"
                return e
              <|> integer

eval' :: String -> Int
eval' xs = case (parse expr' xs) of
              [(n,[])]  -> n
              [(_,out)] -> error ("Unused input " ++ out)
              []        -> error "Invalid input"

main = do
    print $ eval' "10^3"
    print $ eval' "2^2^3" -- 64
    print $ eval' "128+2^2^3*2" -- 256
    print $ eval' "128+(2+2)^3*2" -- 256
    print $ eval' "128+(2+2)^3*2+256" -- 512
