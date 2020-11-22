module Vector where

type Vector     = [Integer]
data Expr       = V Vector
                | VO VectorOp Expr Expr
                | SO ScalarOp IntExpr Expr
data IntExpr    = I Integer
                |NO NormOp Expr
data VectorOp   = Add | Sub | Dot
data ScalarOp   = Mul | Div
data NormOp     = NormOne | NormInf

-- #include "showme.hs"

instance Show Expr where
    show (V x) = show x
    show (VO op e1 e2) = "{"++show op++", "++show e1++", "++show e2++"}"
    show (SO op ie e) = "{"++show op++", "++show ie++", "++show e++"}"
instance Show IntExpr where
    show (I x) = show x
    show (NO op e) = "{"++show op++", "++show e++"}"
instance Show VectorOp where
    show Add = "'add'"
    show Sub = "'sub'"
    show Dot = "'dot'"
instance Show ScalarOp where
    show Mul = "'mul'"
    show Div = "'div'"
instance Show NormOp where
    show NormOne = "'norm_one'"
    show NormInf = "'norm_inf'"
