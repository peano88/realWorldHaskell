import Data.List 

data Op = Plus | Minus | Mul | Div | Pow deriving (Eq, Show)

data SymbolicManip a = 
    Number a
    | Symbol String
    | BinaryArith Op (SymbolicManip a) (SymbolicManip a)
    | UnaryArith String (SymbolicManip a)
    deriving (Eq)

{- Make SymbolicManip an instance of Num -}
instance (Num a) => Num (SymbolicManip a) where
    a + b = BinaryArith Plus a b
    a - b = BinaryArith Minus a b 
    a * b = BinaryArith Mul a b 
    negate a = BinaryArith Mul (Number (-1)) a 
    abs a = UnaryArith "abs" a
    signum _ = error "signum is not implemented"
    fromInteger i = Number (fromInteger i)

{- Make SymbolicManip an instance of Fractional -}
instance (Fractional a) => Fractional (SymbolicManip a) where
    a / b = BinaryArith Div a b
    recip a = BinaryArith Div (Number 1) a
    fromRational r = Number (fromRational r)

{- Make SymbolicManip an instance of Floating -}
instance (Floating a) => Floating (SymbolicManip a) where
    pi = Symbol "pi"
    exp a = UnaryArith "exp" a
    log a = UnaryArith "log" a
    sqrt a = UnaryArith "sqrt" a
    a ** b = BinaryArith Pow a b
    sin a = UnaryArith "sin" a
    cos a = UnaryArith "cos" a
    tan a = UnaryArith "tan" a
    asin a = UnaryArith "asin" a
    acos a = UnaryArith "acos" a
    atan a = UnaryArith "atan" a
    sinh a = UnaryArith "sinh" a
    cosh a = UnaryArith "cosh" a
    tanh a = UnaryArith "tanh" a
    asinh a = UnaryArith "asinh" a
    acosh a = UnaryArith "acosh" a
    atanh a = UnaryArith "atanh" a

instance (Show a, Num a) => Show (SymbolicManip a) where
    show a = prettyShow a 

{- Helper functions -}
op2Str :: Op -> String 
op2Str Plus = "+"
op2Str Minus = "-"
op2Str Mul = "*"
op2Str Div = "/"
op2Str Pow = "^"

simpleParen :: (Show a, Num a) => SymbolicManip a -> String 
simpleParen (Number x) = prettyShow (Number x)
simpleParen (Symbol x) = prettyShow (Symbol x)
simpleParen x@BinaryArith {} = "(" ++ prettyShow x ++ ")" 
simpleParen x@(UnaryArith _ _) = prettyShow x

prettyShow :: (Show a, Num a) => SymbolicManip a -> String 
prettyShow (Number x) = show x
prettyShow (Symbol x) = x
prettyShow (BinaryArith op a b) = 
    let pa = simpleParen a
        pb = simpleParen b 
        pop = op2Str op
    in pa ++ pop ++ pb
prettyShow (UnaryArith op a) = op ++ "(" ++ show a ++ ")"

rpnShow :: (Show a, Num a) => SymbolicManip a -> String 
rpnShow i = 
    let toList (Number x) = [show x]
        toList (Symbol x) = [x]
        toList (BinaryArith op a b) = toList a ++ toList b ++ [op2Str op]
        toList (UnaryArith op a) = toList a ++ [op] 
        join :: [a] -> [[a]] -> [a]
        join delim l = intercalate delim l
   in join " " (toList i)