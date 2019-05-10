-- delete from a list


delete' :: Int -> [Int] -> [Int]
delete' y [] = []
delete' y (x:xs) = if x==y then xs else x : delete' y xs