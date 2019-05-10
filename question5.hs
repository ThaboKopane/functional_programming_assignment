-- exprs
-- returns all expressions whose list


data Expr = Val Int | App Op Expr Expr
data Op = Add | Mul


--printing symbols
myShow :: Op -> String
myShow Add = "+"
myShow Mul = "*"

--print expressions
myExpr :: Expr -> String
myExpr (Val n) = show n
myExpr (App o l r)  = myExpr l ++ myShow o ++ myExpr r

instance Show Expr where
    show = myExpr

split :: [Int] -> [([Int], [Int])]
split [] = [([],[])]
split l = [(take i l, drop i l) | i <- [1..length l-1]]

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

