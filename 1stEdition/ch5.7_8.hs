import Data.Char

letlow2int :: Char -> Int
letlow2int c = ord c - ord 'a'

letupp2int :: Char -> Int
letupp2int c = ord c - ord 'A'

int2letlow :: Int -> Char
int2letlow n = chr(ord 'a' +  n)

int2letupp :: Int -> Char
int2letupp n = chr(ord 'A' +  n)

shift :: Int -> Char -> Char
shift n c | isLower c = int2letlow((letlow2int c+n) `mod` 26)
          | isUpper c = int2letupp((letupp2int c+n) `mod` 26)
          | otherwise = c

encode :: Int -> String -> String
encode n xs = [shift n x| x <- xs]

table :: [Float]
table = [8.2, 1.5, 2.8, 4.3, 12.7, 2.2, 2.0, 6.1, 7.0, 0.2, 0.8, 4.0, 2.4,
         6.7, 7.5, 1.9, 0.1, 6.0, 6.3, 9.1, 2.8, 1.0, 2.4, 0.2, 2.0, 0.1]

percent :: Int -> Int -> Float
percent n m = (fromIntegral n / fromIntegral m) * 100

lowers :: String -> Int
lowers xs = length[x| x <- xs, isLower x]

count :: Char -> String -> Int
count x xs = length[x' | x'<- xs, x==x']

freqs :: String -> [Float]
freqs xs = [percent (count x xs) n | x <-['a'..'z']]
           where n = lowers xs

chisqr :: [Float] -> [Float] -> Float
chisqr os es = sum[((o - e)^2) /e | (o,e) <- zip os es]

rotate :: Int -> [a] -> [a]
rotate n xs = drop n xs ++ take n xs

positions :: Eq a => a -> [a] -> [Int]
positions x xs = [i| (x',i)<- zip xs [0..n], x==x']
                 where n = length xs - 1

crack :: String -> String
crack xs = encode (-factor) xs
  where
    factor = head (positions (minimum chitab) chitab)
    chitab = [chisqr (rotate n table') table | n <- [0..25]]
    table' = freqs [toLower(x)| x <- xs]


main = do
    print $ let2int 'a'
    print $ int2let 0
    print $ shift 3 'z'
    print $ shift (-3) 'c'
    print $ shift 3 ' '
    print $ encode 3 "haskell is fun"
    print $ encode (-3) "kdvnhoo lv ixq"
    print $ lowers "abbcccddddeeeee"
    print $ freqs "abbcccddddeeeee"
    print $ rotate 3 [1,2,3,4,5]
    print $ crack "Kdvnhoo lv ixq"
