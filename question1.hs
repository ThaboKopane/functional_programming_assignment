

--Evaluate expressions down to integer level
--eval :: Expr -> Int



data Expr = Val Int | App Op Expr Expr
data Op = Add | Mul | Sub | Div


-- check validity of operation
valid :: Op -> Int -> Int -> Bool
valid Add _ _ = True
valid Sub x y = x > y
valid Mul _ _ = True
valid Div x y = x `mod` y == 0

-- apply operation
apply :: Op -> Int -> Int -> Int
apply Add x y = x + y
apply Sub x y = x - y
apply Mul x y = x * y
apply Div x y = x `div` y

--eval
eval :: Expr -> [Int]
eval (Val n)  = [n | n > 0]
eval (App o l r) = [ apply o x y | x <- eval l
                                 , y <- eval r
                                 , valid o x y]

-- values
values :: Expr -> [Int]
values (Val n) = [n]
values (App _ l r) = values l ++ values r