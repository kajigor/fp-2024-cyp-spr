module Main where 

import Text.Printf (printf)
import Control.Monad (unless)


-- instance Functor (Either a) where
--   fmap f (Right x) = Right (f x)
--   fmap f (Left x) = (Left x)

-- instance Functor ((->) a) where
--   fmap f g = f . g

data BinOperator = Plus | Minus | Multiply | Divide | Power deriving (Eq)
instance Show BinOperator where
  show Plus = "+"
  show Minus = "-"
  show Multiply = "*"
  show Divide = "/"
  show Power = "^"

data Expr a = Const a | Var String |  SquareRoot (Expr a) | BinExpr BinOperator (Expr a) (Expr a) deriving Eq 

instance (Show a) => Show (Expr a) where 
  show (Const n) = show n 
  show (Var s) = show s
  show (SquareRoot exp) = printf "(square root of %s)"  (show exp)
  show (BinExpr binOp leftExp rightExp) = printf "(%s %s %s)" (show leftExp) (show binOp) (show rightExp)

instance (Num a) => Num (Expr a) where
  (+) = BinExpr Plus
  (*) = BinExpr Multiply
  negate = BinExpr Multiply (Const (-1))
  fromInteger = Const . fromInteger


data Error = DivisionByZero | SquareRootOfNegative | NoSuchVariable deriving Eq 

instance Show Error where 
  show (DivisionByZero) = printf "Division by zero"
  show (SquareRootOfNegative) = printf "Square root of negative number"
  show (NoSuchVariable) = printf "No such variable"


eval :: (Floating a, Ord a) => Expr a -> [(String, a)] -> Either Error a 
eval (Const n) _ = Right n
eval (Var s) xs = evalVarValue s xs
eval (SquareRoot exp) xs = evalSquareRootExp (eval exp xs)
eval (BinExpr binOp exp1 exp2) xs = evalBinExp binOp (eval exp1 xs) (eval exp2 xs) 

evalVarValue :: String ->  [(String, a)] -> Either Error a 
evalVarValue _ [] = Left NoSuchVariable
evalVarValue s (x:xs) = if (s == (fst x)) then Right (snd x) else evalVarValue s xs

evalSquareRootExp :: (Floating a, Ord a) =>  Either Error a -> Either Error a
evalSquareRootExp (Left error) = Left error
evalSquareRootExp (Right n) = if n < 0 then Left SquareRootOfNegative else Right (sqrt n)

evalBinExp ::  (Floating a, Ord a) => BinOperator -> Either Error a ->  Either Error a -> Either Error a
evalBinExp _ (Left err) _ = Left err
evalBinExp _ _ (Left err) = Left err
evalBinExp Plus (Right n1) (Right n2) = Right (n1 + n2)
evalBinExp Minus (Right n1) (Right n2) = Right (n1 - n2)
evalBinExp Multiply (Right n1) (Right n2) = Right (n1 * n2)
evalBinExp Power (Right n1) (Right n2) = Right (n1 ** n2)
evalBinExp Divide (Right n1) (Right n2) = evalDivideExp n1 n2

evalDivideExp :: (Floating a, Ord a) => a ->  a -> Either Error a
evalDivideExp _ 0 = Left DivisionByZero
evalDivideExp n1 n2 = Right (n1 / n2)

-- simplify :: (Num a, Ord a) => Expr a -> Expr a
-- simplify (BinExpr Plus (Const 0) x) = simplify x
-- simplify (BinExpr Plus x (Const 0)) = simplify x
-- simplify (BinExpr Minus x (Const 0)) = simplify x
-- simplify (BinExpr Multiply (Const 1) x) = simplify x
-- simplify (BinExpr Multiply x (Const 1)) = simplify x
-- simplify (BinExpr Multiply (Const 0) _) = Const 0
-- simplify (BinExpr Multiply _ (Const 0)) = Const 0
-- simplify (BinExpr Divide x (Const 1)) = simplify x
-- simplify (BinExpr Power (Const 1) _) = (Const 1)
-- simplify (BinExpr Power (Const 0) _) = (Const 0)
-- simplify (BinExpr Power x (Const 1)) = simplify x
-- simplify (BinExpr Power x (Const 0)) = (Const 1)
-- simplify (BinExpr binOp x y) = simplify (BinExpr binOp (simplify x) (simplify y))
-- simplify (SquareRoot (Const 0)) = (Const 0)
-- simplify (SquareRoot (Const 1)) = (Const 1)
-- simplify (SquareRoot x) = simplify (SquareRoot (simplify x))
-- simplify exp = exp

simplify :: (Num a, Ord a) => Expr a -> Expr a
simplify (BinExpr Plus x y) = simplifyPlus (simplify x) (simplify y)
simplify (BinExpr Minus x y) = simplifyMinus (simplify x) (simplify y)
simplify (BinExpr Multiply x y) = simplifyMultiply (simplify x) (simplify y)
simplify (BinExpr Divide x y) = simplifyDivide (simplify x) (simplify y)
simplify (BinExpr Power x y) = simplifyPower (simplify x) (simplify y)
simplify (SquareRoot x) = simplifySqrt (simplify x)
simplify x = x

simplifyPlus (Const 0) x = x
simplifyPlus x (Const 0) = x
simplifyPlus x y = BinExpr Plus x y

simplifyMinus x (Const 0) = x
simplifyMinus x y = BinExpr Minus x y

simplifyMultiply (Const 1) x = x
simplifyMultiply x (Const 1) = x
simplifyMultiply (Const 0) _ = Const 0
simplifyMultiply _ (Const 0) = Const 0
simplifyMultiply x y = BinExpr Multiply x y

simplifyDivide x (Const 1) = x
simplifyDivide x y = BinExpr Divide x y

simplifyPower (Const 1) _ = Const 1
simplifyPower (Const 0) _ = Const 0
simplifyPower x (Const 0)  = Const 1
simplifyPower x (Const 1)  = x
simplifyPower x y = BinExpr Power x y

simplifySqrt (Const 0) = Const 0
simplifySqrt (Const 1) = Const 1
simplifySqrt x = SquareRoot x

casesEval :: (Floating a, Ord a) => [(Expr a, Either Error a)]
casesEval = [(Const 12, Right 12), 
  (SquareRoot (Const 4), Right 2), 
  (BinExpr Plus (Const 30) (Const 20), Right 50),
  (BinExpr Minus (Const 100) (Const 115.5), Right (-15.5)),
  (BinExpr Multiply (Const 50) (Const 7.5), Right 375),
  (BinExpr Power (Const 2) (Const 4), Right 16),
  (BinExpr Divide (Const 21) (Const 2), Right 10.5),
  (BinExpr Divide (Const 37) (Const 0), Left DivisionByZero),
  (SquareRoot (Const (-24)), Left SquareRootOfNegative),
  (BinExpr Multiply (BinExpr Minus (Const 12) (Const 4)) (SquareRoot (BinExpr Power (Const 20) (Const 2))), Right 160),
  (BinExpr Plus (BinExpr Multiply (Const 12) (Const 4)) (BinExpr Power (Const 2) (Const 5)), Right 80),
  (BinExpr Divide (BinExpr Multiply (Const 150) (Const 50)) (BinExpr Power (Const 0) (Const 2)), Left DivisionByZero),
  (BinExpr Plus (SquareRoot (Const (-345))) (BinExpr Power (Const 100) (Const 4)), Left SquareRootOfNegative),
  (Var "x", Right 1),
  (Var "b", Left NoSuchVariable),
  (BinExpr Plus (Var "y") (Const 20), Right 22),
  (BinExpr Multiply (BinExpr Divide (Const 1) (Const 0)) (SquareRoot (Var "a")), Left DivisionByZero),
  (BinExpr Power (BinExpr Plus (Var "z") (Var "y")) (BinExpr Minus (Var "z") (Var "x")), Right 25)
  ] 


casesSimplify :: (Num a) => [(Expr a, Expr a)]
casesSimplify = [
  (Var "x", Var "x")
  , (Const 10, Const 10)
  , (BinExpr Plus (Const 2) (Const 5), BinExpr Plus (Const 2) (Const 5))
  , (SquareRoot (Var "x"), SquareRoot (Var "x"))
  , (BinExpr Plus (Const 0) (Const 5), (Const 5))
  , (BinExpr Plus (Const 5) (Const 0), (Const 5))
  , (BinExpr Minus (Var "x") (Const 0), (Var "x"))
  , (BinExpr Multiply (Var "x") (Const 0), (Const 0))
  , (BinExpr Multiply (Const 0) (Var "x"), (Const 0))
  , (BinExpr Multiply (Const 1) (Var "x"), (Var "x"))
  , (BinExpr Multiply  (Var "x") (Const 1), (Var "x"))
  , (BinExpr Divide  (Var "x") (Const 1), (Var "x"))
  , (BinExpr Power  (Var "x") (Const 1), (Var "x"))
  , (BinExpr Power  (Var "x") (Const 0), (Const 1))
  , (BinExpr Power  (Const 1) (Const 20), (Const 1))
  , (BinExpr Power  (Const 0) (Const 20), (Const 0))
  , (SquareRoot (Const 0), Const 0)
  , (SquareRoot (Const 1), Const 1)
  , (BinExpr Multiply (Const 1) (BinExpr Plus (Const 0) (Var "x")), Var "x")
  , (BinExpr Multiply (Const 1) (BinExpr Minus (Var "x") (SquareRoot (Const 0))), Var "x")
  , (BinExpr Minus (BinExpr Power (Var "x") (Const 1)) (BinExpr Divide (Const 2) (Const 1)), BinExpr Minus (Var "x") (Const 2))
  , (BinExpr Multiply (BinExpr Multiply (Const 0) (Const 1)) (BinExpr Multiply (Const 1) (Const 0)), Const 0)
  ]


testEval :: (Floating a, Ord a, Show a) => Expr a -> Either Error a -> IO () 
testEval expr expected = 
    let actual = eval expr variables in 
    unless (expected == actual) $ describeFailure actual
  where
    variables = [("x", 1), ("y", 2), ("z", 3)] 
    describeFailure actual = 
      printf "eval (%s) should be %s but it was %s" (show expr) (show expected) (show actual) 
  

testSimplify :: (Floating a, Ord a, Show a) => Expr a -> Expr a -> IO () 
testSimplify expr expected = 
    let actual = simplify expr in 
    unless (expected == actual) $ describeFailure actual
  where
    describeFailure actual = 
      printf "simplify (%s) should be %s but it was %s" (show expr) (show expected) (show actual) 
  


main :: IO () 
main = do
  mapM_ (uncurry testEval) casesEval
  mapM_ (uncurry testSimplify) casesSimplify

  