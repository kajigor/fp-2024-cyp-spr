module Main where 

import Text.Printf (printf)
import Control.Monad (unless)

-- Part 1

data Either' a b = Left' a | Right' b

instance Functor (Either' a) where
  fmap f (Left' x) = Left' x
  fmap f (Right' x) = Right' (f x)

-- Laws to be proven
-- 1.
-- fmap id == id
-- Proof:
-- fmap id (Left' x)  := Left' x; ok
-- fmap id (Right' x) := Right' (id x) == Right' x; ok
--
-- 2.
-- fmap (f . g) == fmap f . fmap g
-- Proof:
-- fmap (f . g) (Left' x) := Left' x,
-- (fmap f . fmap g) (Left' x) == fmap f (fmap g (Left' x)) := fmap f (Left' x) := Left' x; ok
-- fmap (f . g) (Right' x) := Right' ((f . g) x) == Right' (f (g x)),
-- (fmap f . fmap g) (Right' x) == fmap f (fmap g (Right' x)) := fmap f (Right' (g x)) := Right' f (g x); ok

-- Part 2

newtype Arrow a b = Arrow ((->) a b)

instance Functor (Arrow a) where
  fmap f (Arrow g) = Arrow (f . g)

-- Laws to be proven
-- 1.
-- fmap id == id
-- Proof:
-- fmap id (Arrow f) := Arrow (id . f) == Arrow f; ok
--
-- 2.
-- fmap (f . g) == fmap f . fmap g
-- Proof:
-- fmap (f . g) (Arrow h) := Arrow ((f . g) . h) == Arrow (f . g . h),
-- (fmap f . fmap g) (Arrow h) := fmap f (Arrow (g . h)) := Arrow (f . (g . h)) = Arrow (f . g . h); ok

data BinOp = Plus | Minus | Mult | Div | Exp deriving (Eq)
instance Show BinOp where
  show Plus  = "+"
  show Minus = "-"
  show Mult  = "*"
  show Div   = "/"
  show Exp   = "^"

data FunOpUn = Sqrt deriving (Eq)
instance Show FunOpUn where
  show Sqrt = "sqrt"

data Expr a = Variable String | Number a | Binary BinOp (Expr a) (Expr a) | UnaryFun FunOpUn (Expr a) deriving (Eq)
instance Show a => Show (Expr a) where
  show (Variable s) = show s
  show (Number x) = show x
  show (Binary op lhs rhs) = '(' : show lhs ++ show op ++ show rhs ++ ")"
  show (UnaryFun fun arg) = show fun ++ "(" ++ show arg ++ ")"

data Error = UndefinedVar | DivByZero | NegExponent | NegSqrt deriving (Eq)

instance Show Error where 
  show UndefinedVar = "Trying to get a value of an undefined variable"
  show DivByZero = "Division by zero"
  show NegExponent = "Trying to exponentiate negative value"
  show NegSqrt = "Trying to take a square root of a negative number"

getFun :: (Ord a, Floating a) => BinOp -> (a -> a -> a)
getFun Plus   = (+)
getFun Minus  = (-)
getFun Mult   = (*)
getFun Div    = (/)
getFun Exp    = (**)

evalBinOp :: (Ord a, Floating a) => BinOp -> Either Error a -> Either Error a -> Either Error a
evalBinOp _ (Left e) _ = Left e
evalBinOp _ _ (Left e) = Left e
evalBinOp op@Div (Right lhs) (Right rhs)  = if (rhs == 0) then Left DivByZero else Right $ getFun op lhs rhs
evalBinOp op@Exp (Right lhs) (Right rhs)  = if (lhs < 0)  then Left NegExponent else Right $ getFun op lhs rhs
evalBinOp op (Right lhs) (Right rhs)      = Right $ getFun op lhs rhs

evalFunUn :: (Ord a, Floating a) => FunOpUn -> Either Error a -> Either Error a
evalFunUn _ (Left e) = Left e
evalFunUn Sqrt (Right arg) = if (arg < 0) then Left NegSqrt else Right $ sqrt arg

resolve :: (Ord a, Floating a) => [(String, a)] -> String -> Either Error a
resolve [] _ = Left UndefinedVar
resolve ((key, value):memory) name = if (name == key) then Right value else resolve memory name

eval :: (Ord a, Floating a) => Expr a -> [(String, a)] -> Either Error a
eval (Number x) _ = Right x
eval (Variable s) memory = resolve memory s
eval (Binary op lhs rhs) memory = evalBinOp op (eval lhs memory) (eval rhs memory)
eval (UnaryFun fun arg) memory = evalFunUn fun (eval arg memory)

simplifyIn :: (Ord a, Floating a) => Expr a -> Expr a
simplifyIn (Binary Plus (Number 0) e) = e
simplifyIn (Binary Plus e (Number 0)) = e
simplifyIn (Binary Mult (Number 0) e) = 0
simplifyIn (Binary Mult e (Number 0)) = 0
simplifyIn (Binary Mult (Number 1) e) = e
simplifyIn (Binary Mult e (Number 1)) = e
simplifyIn (Binary Minus e (Number 0)) = e
simplifyIn (Binary Div e (Number 1)) = e
simplifyIn (Binary Exp e (Number 1)) = e
simplifyIn e = e

simplify :: (Ord a, Floating a) => Expr a -> Expr a
simplify (Binary op lhs rhs) = simplifyIn (Binary op (simplify lhs) (simplify rhs))
simplify (UnaryFun fun arg) = UnaryFun fun (simplify arg)
simplify e = e

instance (Num a) => Num (Expr a) where
  (+) = Binary Plus
  (*) = Binary Mult
  (-) = Binary Minus -- why not
  negate = Binary Minus (Number 0)
  fromInteger = Number . fromInteger

cases1 :: [(Expr Double, Either Error Double)]
cases1 = [(Number 1.0, Right 1.0),
         (Number 0.0, Right 0.0),
         (Number 3, Right 3.0),
         (Number 1e300, Right 1e300),
         (Binary Plus (Number 2) (Number 5), Right 7.0),
         (Binary Minus (Number 5) (Number 7), Right (-2.0)),
         (Binary Mult (Number 7.0) (Number 3), Right 21.0),
         (Binary Div (Number 27) (Number 3), Right 9.0),
         (Binary Div (Number 3) (Number 2), Right 1.5),
         (Binary Div (Number 4) (Number 0), Left DivByZero),
         (Binary Exp (Number 2) (Number 4), Right 16.0),
         (Binary Exp (Number (-2.7)) (Number 1.5), Left NegExponent),
         (UnaryFun Sqrt (Number 81.0), Right 9.0),
         (UnaryFun Sqrt (Number (-1.0)), Left NegSqrt),
         (Binary Plus (Number 2) (Binary Mult (Number 2) (Number 2)), Right 6.0),
         (Binary Div (Number 4) (Binary Minus (Number 2) (Number 2)), Left DivByZero)]

test1 :: (Ord a, Floating a, Show a) => Expr a -> Either Error a -> IO () 
test1 expr expected = 
    let actual = eval expr [] in 
    unless (expected == actual) $ describeFailure actual
  where 
    describeFailure actual = 
      printf "eval (%s) should be %s but it was %s" (show expr) (show expected) (show actual) 
  

casesMemory :: [((Expr Double, Either Error Double), [(String, Double)])]
casesMemory = [
                ((Number 0.0, Right 0.0), [("x", 1)])
              , ((Variable "x", Right 1), [("x", 1)])
              , ((Variable "x", Right (-1.0)), [("x", -1.0), ("y", -4)])
              , ((Variable "y", Right (-4)), [("x", -1.0), ("y", -4)])
              , ((Variable "z", Left UndefinedVar), [("x", -1.0), ("y", -4)])
              , (((Variable "x") + (Variable "y"), Right 5), [("x", 2), ("y", 3)])
              , (((Variable "x") - (Variable "y"), Right (-1)), [("x", 2), ("y", 3)])
              , ((0 * ((Variable "x") - (Variable "z")), Left UndefinedVar), [("x", 2), ("y", 3)])
              ]

testWithMemory :: (Ord a, Floating a, Show a) => Expr a -> Either Error a -> [(String, a)] -> IO ()
testWithMemory expr expected memory =
    let actual = eval expr memory in
    unless (expected == actual) $ describeFailure actual
  where
    describeFailure actual = 
      printf "eval (%s) should be %s but it was %s" (show expr) (show expected) (show actual) 

casesSimplify :: [(Expr Double, Expr Double)]
casesSimplify = [
                  (Binary Plus (Number 0) (Number 1), Number 1)
                , (Binary Plus (Number 0) (Binary Plus (Number 0) (Number 1)), Number 1)
                , (0 + (Variable "x"), Variable "x")
                , ((Variable "x") + 0, Variable "x")
                , ((Variable "x") * 0, 0)
                , (0 * (Variable "Wow, I'm not a char!"), 0)
                , ((Variable "x" + 15) * 0, 0)
                , ((Variable "z" * 0) + 3, 3)
                , (1 * Variable "x", Variable "x")
                , ((Variable "x" + 0) * 1, Variable "x")
                , (Variable "x" - 0, Variable "x")
                , (Binary Div (Variable "x") 1, Variable "x")
                , (Binary Exp (Variable "x") 1, Variable "x")
                ]

testSimplify :: (Ord a, Floating a, Show a) => Expr a -> Expr a -> IO ()
testSimplify expr expected =
    let actual = simplify expr in
    unless (expected == actual) $ describeFailure actual
  where
    describeFailure actual = 
      printf "eval (%s) should be %s but it was %s" (show expr) (show expected) (show actual) 

main :: IO () 
main = do 
  mapM_ (uncurry test1) cases1 
  mapM_ ((uncurry . uncurry) testWithMemory) casesMemory
  mapM_ (uncurry testSimplify) casesSimplify
  putStrLn "Done"
  
