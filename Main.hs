import Text.Printf (printf)
import Control.Monad (unless)


data Either1 a b = Left1 a | Right1 b


instance Functor (Either1 a) where
  fmap :: (b -> c) -> Either1 a b -> Either1 a c
  fmap _ (Left1 a) = Left1 a
  fmap f (Right1 b) = Right1 (f b)

{- proof:
  id-law : obviously
  
  composition:

  1.1) fmap (f . g) (Left1 a) = Left1 a 
  1.2) (fmap f . fmap g) (Left1 a) = fmap f (fmap g (Left1 a)) = fmap f (Left1 a) = Left1 a

  2.1) fmap (f . g) (Right1 b) = Right1 ((f.g) b)
  2.2) (fmap f . fmap g) (Right1 a) = fmap f (fmap g (Right1 b)) = fmap f (Right1 (g b)) = Right1 ( (f.g) b)
-}


newtype ArrowT a b = ArrowT {getArrowT :: a -> b} 
instance Functor (ArrowT a) where
  fmap :: (b -> c) -> ArrowT a b ->  ArrowT a c
  fmap f at = ArrowT (f  . getArrowT at)

{- proof:
  id-law: obviously
  
  composition:

  fmap (f . g) ar ~ f . g . ar
  (fmap f . fmap g) ar ~ fmap f (fmap g ar) ~  fmap f (g . ar) ~ f . g . ar

-}


-- a :: (Floating a, Ord a)
data Expr a = Const a 
            | SquareRoot (Expr a)
            | Plus (Expr a) (Expr a)
            | Minus (Expr a) (Expr a)
            | Mult (Expr a) (Expr a)
            | Div (Expr a) (Expr a)
            | Pow (Expr a) (Expr a) 
            | Var String
          deriving Eq


instance Show a => Show (Expr a) where 
  show e = case e of
          (Const c)       -> show c
          (SquareRoot e1)  -> printf "sqrt(%s)" (show e1)
          (Plus e1 e2)    -> printf "(%s %s %s)" (show e1) "+" (show e2)
          (Minus e1 e2)   -> printf "(%s %s %s)" (show e1) "-" (show e2)
          (Mult e1 e2)    -> printf "(%s %s %s)" (show e1) "*" (show e2)
          (Div e1 e2)     -> printf "(%s %s %s)" (show e1) "/" (show e2)
          (Pow e1 e2)     -> printf "(%s %s %s)" (show e1) "^" (show e2)
          (Var v)         -> printf "%s" (show v)


data Error a = DivisionByZero (Expr a) (Expr a) | RootOfNegative (Expr a) | UndefinedVariable String
            deriving Eq

instance Show a => Show (Error a) where 
  show (DivisionByZero e1 e2) = printf "Error when dividing %s by %s. Denominator was equal to zero" (show e1) (show e2)
  show (RootOfNegative e)     = printf "Error when taking square root. Expression %s was negative" (show e)
  show (UndefinedVariable v)  = printf "Undefined variable %s" v
 

eval :: (Floating a, Ord a) => Expr a -> [(String, a)] -> Either (Error a) a 
eval e ls = case e of
  Const c      ->  Right c
  Plus e1 e2   -> perfOp (\ x y -> Right $ x+y ) e1 e2
  Minus e1 e2  -> perfOp (\ x y -> Right $ x-y ) e1 e2
  Mult e1 e2   -> perfOp (\ x y -> Right $ x*y ) e1 e2
  Div e1 e2    -> perfOp (\ x y -> if y /= 0 then Right (x/y) else Left (DivisionByZero e1 e2)) e1 e2
  Pow e1 e2   -> perfOp (\ x y -> Right $ x** y ) e1 e2
  SquareRoot e1 -> perfOp (\_ v -> if v >=0 then Right (sqrt v) else Left (RootOfNegative e1)) e1 e1
  Var v         -> findVariableValue v ls
  where perfOp op e1 e2 = case (eval e1 ls, eval e2 ls) of
                    (Right v1, Right v2) -> op v1 v2
                    (Left er, _) -> Left er
                    (_, Left er) -> Left er

        findVariableValue v [] = Left (UndefinedVariable v)
        findVariableValue name ((var, val):rest)
          | var == name = Right val
          | otherwise = findVariableValue name rest

type ExprConstCaseType = Double

cases :: [(Expr ExprConstCaseType, [(String, ExprConstCaseType)] , Either (Error ExprConstCaseType) ExprConstCaseType)]
cases = [ 
        (Const 777, [],  Right 777),
        (Pow (Div (Mult (Minus (Pow (Plus (SquareRoot(Const 25)) (Const 3)) (Const 2)) (Mult (Const 4) (Const 7))) (Const 2)) (Const 5)) (Const 3), [],  Right ( ((  ( ( sqrt 25 + 3)**2 - 4 * 7) * 2) / 5)**3)),  
        let e1 = Mult (Const 4) (Const 5)
            e2 = Plus (Const 1) (Const (-1)) in
            (Div e1 e2, [],  Left (DivisionByZero e1 e2)),
        let e = Minus (Const 1) (Const 100) in
        (SquareRoot e, [],  Left (RootOfNegative e)),
        (Var "x", [("x", 100)], Right 100),
        (Plus (Var "x") (Var "y"), [("x", 10.0), ("y", 20.0)], Right 30.0),
        (Plus (Var "z") (Var "y"), [("x", 10), ("y", 20)], Left $ UndefinedVariable "z")
        ] 

test :: Expr ExprConstCaseType -> [(String, ExprConstCaseType)]  -> Either (Error ExprConstCaseType) ExprConstCaseType -> IO () 
test expr var_list expected = 
    let actual = eval expr var_list in 
    unless (expected == actual) $ describeFailure actual
  where 
    describeFailure actual = 
      printf "eval (%s) should be %s but it was %s\n" (show expr) (show expected) (show actual) 


simplify :: (Floating a, Ord a) => Expr a -> Expr a

simplify e = if trySimplifyResult /= e then simplify trySimplifyResult else trySimplifyResult where
  trySimplifyResult = trySimplify e

  trySimplify (Plus (Const 0) expr) = trySimplify expr
  trySimplify (Plus expr (Const 0)) = trySimplify expr 

  trySimplify (Mult (Const 0) _) = Const 0
  trySimplify (Mult _ (Const 0)) = Const 0

  trySimplify (Mult (Const 1) expr) = trySimplify expr  
  trySimplify (Mult expr (Const 1)) = trySimplify expr

  trySimplify (Div l (Const 1)) = trySimplify l

  trySimplify (Minus l (Const 0)) = trySimplify l

  trySimplify (Mult expr1 expr2) = Mult (trySimplify expr1) (trySimplify expr2)
  trySimplify (Plus expr1 expr2) = Plus (trySimplify expr1) (trySimplify expr2) 
  trySimplify (Minus expr1 expr2) = Minus (trySimplify expr1) (trySimplify expr2)
  trySimplify expr = expr



{-
simplify :: (Eq a, Fractional a) => Expr a -> Expr a
simplify e = if trySimplify /= e then simplify trySimplify else trySimplify
  where 
        rule1 (Mult (Const 0) _) = Const 0
        rule1 (Mult _ (Const 0)) = Const 0
        rule1 ex                 = ex 

        rule2 (Plus (Const 0) r) = r
        rule2 (Plus l (Const 0)) = l
        rule2 ex                 = ex 

        rule3 (Mult (Const 1) r) = r
        rule3 (Mult l (Const 1)) = l
        rule3 ex                 = ex

        rule4 (Div l (Const 1)) = l
        rule4 ex                 = ex  

        rule5 (Minus (Const 0) r) = r
        rule5 (Minus l (Const 0)) = l
        rule5 ex                 = ex 

        trySimplify = foldl (<$>) id [rule1, rule2, rule3, rule4, rule5] e
-}


simplifyTests :: [(Expr ExprConstCaseType, Expr ExprConstCaseType)]
simplifyTests = [ (Mult (Const 0) (Const 5), Const 0),
          (Plus (Mult (Const 1) (Var "x")) (Const 0), Var "x"),
          (Plus (Const 0) (       
                Mult  (Const 1) (    
                      Minus (Div (Const 42) (Const 1) ) (Const 0) 
                )
                )
        , Const 42), 
        (Plus (Const 1) (Mult (Const 0) (Var "x") ), Const 1)]


testSimplify :: Expr ExprConstCaseType -> Expr ExprConstCaseType-> IO () 
testSimplify exprToSimplify expectedExpr = 
    let actual = simplify exprToSimplify in 
    unless (expectedExpr == actual) $ describeFailure actual
  where 
    describeFailure actual = 
      printf "simplify (%s) should be %s but it was %s\n" (show exprToSimplify) (show expectedExpr) (show actual) 

instance Num a => Num (Expr a) where
  (+) = Plus
  (*) = Mult
  negate = Minus (Const 0) 
  fromInteger = Const . fromInteger

main :: IO () 
main = do 
  mapM_ (\(expr, expected, vars) -> test expr expected vars) cases
  mapM_ (uncurry testSimplify) simplifyTests
