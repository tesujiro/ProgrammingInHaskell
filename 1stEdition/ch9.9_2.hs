import Data.Char

-- 8.2
--type Parser a = String -> [(a, String)]
newtype Parser a = P (String -> [(a,String)])

-- 8.3
failure :: Parser a
failure = P(\inp -> [])

{-
item :: Parser Char
item = \inp -> case inp of
                 [] -> []
                 (x:xs) -> [(x,xs)]
-}
item :: Parser Char
item = P (\inp -> case inp of
                     []     -> []
                     (x:xs) -> [(x,xs)])

{-
parse   :: Parser a -> String -> [(a, String)]
parse p inp = p inp
-}
parse :: Parser a -> String -> [(a,String)]
parse (P p) inp = p inp

-- 8.5
(+++) :: Parser a -> Parser a -> Parser a
p +++ q = P(\inp -> case parse p inp of
                      [] -> parse q inp
                      [(v, out)] -> [(v, out)])

-- 8.6
sat :: (Char -> Bool) -> Parser Char
sat p = do x <- item
           if p x then return x else failure

digit :: Parser Char
digit = sat isDigit

lower :: Parser Char
lower = sat isLower

alphanum :: Parser Char
alphanum = sat isAlphaNum

char :: Char -> Parser Char
char x = sat (==x)

string :: String -> Parser String
string [] = return []
string (x:xs) = do char x
                   string xs
                   return (x:xs)

many :: Parser a -> Parser [a]
many p = many1 p +++ return []
many1 :: Parser a -> Parser [a]
many1 p = do v <- p
             vs <- many p
             return (v:vs)

ident :: Parser String
ident = do x <- lower
           xs <- many alphanum
           return (x:xs)

nat :: Parser Int
nat = do xs <- many1 digit
         return (read xs) 

space :: Parser ()
space = do many (sat isSpace)
           return ()

-- 8.7
token :: Parser a -> Parser a
token p = do space
             v <- p
             space
             return v

natural :: Parser Int
natural = token nat

symbol :: String -> Parser String
symbol xs = token ( string xs )

-- 8.8
expr :: Parser Int
expr = do t <- term
          do symbol "+"
             e <- expr
             return (t + e)
           +++ return t

term :: Parser Int
term = do f <- factor
          do symbol "*"
             t <- term
             return (f*t)
           +++ return f

factor :: Parser Int
factor = do symbol "("
            e <- expr
            symbol ")"
            return e
          +++ natural

-- 9.5
type Pos = (Int, Int)

beep :: IO ()
beep = putStr "\BEL"

cls :: IO ()
cls = putStr "\ESC[2J"

goto    :: Pos -> IO ()
goto (x ,y) = putStr ("\ESC[" ++ show y ++ ";" ++ show x ++ "H")

writeat :: Pos -> String -> IO ()
writeat p xs = do goto p
                  putStr xs

seqn    :: [IO a ] -> IO ()
seqn [] = return ()
seqn (a:as) = do a
                 seqn as
-- 9.6
box :: [String]
box = ["+---------------+",
       "| q | c | d | = |",
       "+---+---+---+---+",
       "| 1 | 2 | 3 | + |",
       "+---+---+---+---+",
       "| 4 | 5 | 6 | - |",
       "+---+---+---+---+",
       "| 7 | 8 | 9 | * |",
       "+---+---+---+---+",
       "| 0 | ( | ) | / |",
       "+---+---+---+---+"]

buttons :: [Char]
buttons = standard ++ extra
          where
            standard = "qcd=123+456+789+0()/"
            extra    = "QCD \ESC\BS\DEL\n"

showbox :: IO ()
showbox = seqn [ writeat (1,y) xs | (y,xs) <- zip [1..13] box]

display :: String -> IO ()
display xs = do writeat (3, 2) "                 "
                writeat (3,2) (reverse ( take 13 (reverse xs)))

calc :: String -> IO ()
calc xs = do display xs
             c <- getChar
             if elem c buttons then
                  process c xs
                else
                  do beep
                     calc xs

process :: Char -> String -> IO ()
process c xs
  | elem c "qQ\ESC"      = quit
  | elem c "dD\BS\DEL"   = delete xs
  | elem c "=\n"         = eval xs
  | elem c "cC"          = clear
  | otherwise            = press c xs

quit :: IO ()
quit = goto (1, 14)

delete :: String -> IO ()
delete "" = calc ""
delete xs = calc ( init xs )

eval :: String -> IO ()
eval xs = case parse expr xs of
            [(n, "")] -> calc ( show n )
            _ -> do beep
                    calc xs

clear :: IO ()
clear = calc ""

press :: Char -> String -> IO ()
press c xs = calc (xs ++ [c])

main = do
     cls
     showbox
