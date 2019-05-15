import Data.Char

-- this function converts a lower-case a-z letter 
-- to an integer between 0-25.

let2int :: Char -> Int
let2int c = ord c - ord 'a'

--This function converts a number from 0-25
--into a letter from a -z

int2let :: Int -> Char
int2let n = chr (ord 'a' +n)

-- define a shift function that applies a factor

shift :: Int -> Char -> Char
shift n c | isLower c = int2let ((let2int c + n) `mod` 26)
           | otherwise = c


-- Define a function that encodes a string
-- using a given shift factor

encode :: Int -> String -> String
encode n xs = [shift n x | x <- xs]

--  function that calculats the % of integer

percent n m = (fromIntegral n / fromIntegral m) * 100

-- frequency usinng percent
freqs :: String -> [Float]
freqs xs = [ percent (count x xs) n | x <- ['a'..'z']]
            where n = lowers xs