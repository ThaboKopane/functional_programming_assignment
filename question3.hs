-- permutations

-- Delete function to implement perms
delete' :: Int -> [Int] -> [Int]
delete' y [] = []
delete' y (x:xs) = if x==y then xs else x : delete' y xs

-- perms
perms :: [Int] -> [[Int]]
perms [] = [[]]
perms xs = do x <- xs
              let n = delete' x xs
              ns <- perms n
              return $ x : ns