import Data.Char

type Bit = Int

bin2int :: [Bit] -> Int
bin2int bits = sum [ w * b | (w, b) <- zip weights bits]
               where weights = iterate (*2) 1

int2bin   :: Int -> [Bit]
int2bin 0 = []
int2bin n = n `mod` 2 : int2bin ( n `div` 2)

make8 :: [Bit] -> [Bit]
make8 bits = take 8 ( bits ++ repeat 0)

encode :: String -> [Bit]
encode = concat . map (make8.int2bin.ord)

chop8 :: [Bit] -> [[Bit]]
chop8 [] = []
chop8 bits = take 8 bits : chop8 ( drop 8 bits )

decode :: [Bit] -> String
decode = map (chr.bin2int) . chop8

transmit :: String -> String
transmit = decode . channel . encode

channel :: [Bit] -> [Bit]
channel = id

parity :: [Bit] -> Bit
parity = foldl (\x y -> (x + y) `mod` 2) 0

addParity :: [Bit] -> [Bit]
addParity bits = bits ++ [parity bits]

rmParity :: [Bit] -> [Bit]
rmParity bits = if parity (take (length bits -1) bits) == last bits then take (length bits -1) bits else error ("parity error :" ++ show bits)

chop9 :: [Bit] -> [[Bit]]
chop9 [] = []
chop9 bits = take 9 bits : chop9 ( drop 9 bits )

encode' :: String -> [Bit]
encode' = concat . map (addParity . make8 . int2bin . ord)

decode' :: [Bit] -> String
decode' = map (chr.bin2int) . (map rmParity) . chop9

transmit' :: String -> String
transmit' = decode' . channel . encode'

-- excercise 9

channel' :: [Bit] -> [Bit]
channel' = concat . map tail . chop9

transmit'' :: String -> String
transmit'' = decode' . channel' . encode'

main = do
    print $ encode "abc"
    print $ decode [1, 0, 0, 0, 0, 1, 1, 0, 0, 1, 0, 0, 0, 1, 1, 0, 1, 1, 0, 0, 0, 1, 1, 0]
    print $ transmit "higher-order functions are easy"
    print $ parity [1, 0, 0]
    print $ parity [1, 0, 0, 1]
    print $ addParity [1, 0, 0, 1]
    print $ rmParity [1, 0, 0, 1, 0]
    -- print $ rmParity [1, 0, 0, 1, 1] -- parity error
    print $ encode' "abc"
    print $ decode' [1,0,0,0,0,1,1,0,1,0,1,0,0,0,1,1,0,1,1,1,0,0,0,1,1,0,0]
    print $ transmit' "higher-order functions are easy"
    print $ channel' [1,0,0,0,0,1,1,0,1,0,1,0,0,0,1,1,0,1,1,1,0,0,0,1,1,0,0]
    -- excercise 9
    print $ transmit'' "higher-order functions are easy" -- parity error
