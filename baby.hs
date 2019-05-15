doubleSmallNumber x = if x > 100
                        then x
                        else x*2


-- Using lists in haskell
-- list comprehensions

boomBangs xs = [if x < 10 then "BOOM!" else "BANG!" | x <-xs, odd x]

-- remove uppercase with list comprehensions

removeNonUppercase st = [c | c  <- st, c `elem` ['A'..'Z']]

--factroail with type declaration
factorial :: Integer -> Integer
factorial n = product [1..n]

-- check if a number we supplied is 7 or not

lucky :: (Integral a) => a -> String
lucky 7 = "LUCKY NUMBER SEVEN!"
lucky x = "Sorry, you're out of luck,pal!"


-- Our own head function

head' :: [a] -> a
head' [] = error "Can't call head on an empty list"
head' (x:_) = x

-- our own length function

length' :: (Num b) => [a] -> b
length' [] = 0
length' (_:xs) = 1 + length' xs


-- bMI beration.

bmiTell :: (RealFloat a) => a -> a -> String
bmiTell weight height
    | bmi <= skinny = "You're underweight, don't die of starvation please!"
    | bmi <= normal = "Apparently you're normal, how boring"
    | bmi <= fat = "So you're getting a bit bigger, we know short people grow wide - but this is too wide"
    | otherwise = "uhm, don't die"
    where bmi = weight/ height^2
          (skinny, normal, fat) = (18.5, 25.0, 30.0)

-- Let bindings 
cylinder :: (RealFloat a) => a -> a -> a
cylinder r h =
    let sideArea = 2 * pi * r * h
        topArea = pi * r^2
    in  sideArea + 2 * topArea

-- RECURSION
-- our own defintition of maximum.

maximum' :: (Ord a) => [a] -> a
maximum' [] = error "maximum of an empty list"
maximum' [x] = x
maximum' (x:xs)
    | x > maxTail = x
    | otherwise = maxTail
    where maxTail = maximum' xs



-- HIGHER ORDER FUNCTIONS
compareWith100 :: (Num a, Ord a) => a -> Ordering
compareWith100 = compare 100

-- Higher orders
applyTwice :: (a -> a) -> a -> a
applyTwice f x = f (f x)

--multiply by threee

multThree :: (Num a) => a -> a -> a -> a
multThree x y z = x * y * z

-- A fucntion that takes a function a two lists and applies the fucntion
-- to the two lists

zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ [] _ = []
zipWith' _ _ [] = []
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys