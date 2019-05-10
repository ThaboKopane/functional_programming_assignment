--Define a functon split.

--this other way
split :: [Int] -> [([Int], [Int])]
split [] = [([],[])]
split l = [(take i l, drop i l) | i <- [1..length l-1]]


-- permutations

-- Delete function to implement perms
delete' :: Int -> [Int] -> [Int]
delete' y [] = []
delete' y (x:xs) = if x==y then xs else x : delete' y xs




