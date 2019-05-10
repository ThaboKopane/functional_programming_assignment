-- 1. A function that prooduces the product of list numbers.

product' :: (Num a) => [a] -> a
product' [] = error "The list is empty"
product' [x] = x
product' (x:xs) = x * product' xs
-- The library function last selects the last element of a
--  a nin empty list.


lastE :: (Num a) => [a] -> a
lastE [] = error "The list is empty"
lastE [x] = x
lastE (x:xs) = lastE xs


-- A function halve, thta splits an even length into two halves

halve :: [a] -> ([a], [a])
halve [] = ([],[])
halve [x] = ([x], [])
halve (x:y:xys) = (x:xs, y:ys) where (xs, ys) = halve xys



-- Null
-- null' :: [a] -> Bool
-- null' (x:xs) = if length xs == 0 then True else False

-- Conditional safetail
-- doesn't work wel
safetailc :: [a] -> [a]
safetailc [] = []
safetailc [x] = [x]
safetailc (x:xs) = if null (x:xs) then [] else tail (x:xs)


-- Pattern mathcing 
safetailp :: [a] -> [a]
safetailp [] = []
safetailp (x:xs) = tail (x:xs)


-- Guarded expressions
safetailg :: [a] -> [a]
safetailg (x:xs)
   | not (null (x:xs))  = tail (x:xs)
   | otherwise    = []