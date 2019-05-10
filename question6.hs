

data Expr = Val Int | App Op Expr Expr
data Op = Add | Mul
type Result = (Expr, Int)

--printing symbols
myShow :: Op -> String
myShow Add = "+"
myShow Mul = "*"

--print expressions
myExpr :: Expr -> String
myExpr (Val n) = show n
myExpr (App o l r)  = bracket l ++ myShow o ++ bracket r
                      where
                        bracket (Val n) = show n
                        braclet e = "(" ++ myShow e ++ ")"

instance Show Expr where
    show = myExpr

-- check validity of operation
valid :: Op -> Int -> Int -> Bool
valid Add _ _ = True
valid Mul _ _ = True

-- apply operation
apply :: Op -> Int -> Int -> Int
apply Add x y = x + y
apply Mul x y = x * y

--eval
eval :: Expr -> [Int]
eval (Val n)  = [n | n > 0]
eval (App o l r) = [ apply o x y | x <- eval l
                                 , y <- eval r
                                 , valid o x y] 



-- split
split :: [Int] -> [([Int], [Int])]
split d = [(take i d, drop i d) | i <- [1..length d-1]]

--comnbine xpressions
combine :: Expr -> Expr -> [Expr]
combine l r = [App o l r | o <- [Add, Mul]]

-- exprs
exprs :: [Int] -> [Expr]
exprs []  = []
exprs [n] = [Val n]
exprs ns  = [e | (ls, rs) <- split ns
               , l        <- exprs ls
               , r        <- exprs rs
               , e        <- combine l r]


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


--subs
subs :: [Int] -> [[Int]]
subs [] = [[]]
subs (x:xs) = ys ++ map (x:) ys
                where
                    ys = subs xs


-- choices
choices :: [Int] -> [[Int]]
choices xs = concat (map perms (subs xs))



-- a solution function
solve :: [Int] -> Int -> [Expr]
solve ns n = [e | ns' <- choices ns
                , e   <- exprs ns'
                , eval e == [n]]
