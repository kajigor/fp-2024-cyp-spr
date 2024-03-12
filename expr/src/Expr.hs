module Expr where
import Text.Printf (printf)
import qualified Data.Map.Strict as M

data Expr a = Const a 
        | SquareRoot (Expr a)
        | Plus (Expr a) (Expr a)
        | Minus (Expr a) (Expr a)
        | Mult (Expr a) (Expr a)
        | Div (Expr a) (Expr a)
        | Pow (Expr a) (Expr a) 
        | Var String
        deriving Eq

instance Num a => Num (Expr a) where
  (+) = Plus
  (*) = Mult
  negate = Minus (Const 0) 
  fromInteger = Const . fromInteger

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

data Error a = DivisionByZero (Expr a) (Expr a) | RootOfNegative (Expr a) | UndefinedVariable String | InvalidMapping [(String, a)] deriving Eq

instance Show a => Show (Error a) where 
  show (DivisionByZero e1 e2) = printf "Error when dividing %s by %s. Denominator was equal to zero" (show e1) (show e2)
  show (RootOfNegative e)     = printf "Error when taking square root. Expression %s was negative" (show e)
  show (UndefinedVariable v)  = printf "Undefined variable %s" v
  show (InvalidMapping ls)    = printf "Incorrect mapping of variables. The following variables have multiple values: %s" (show ls)

eval :: (Floating a, Ord a) => Expr a -> M.Map String a -> Either (Error a) a 
eval e ls = case e of
                Const c      ->  Right c
                Plus e1 e2   -> perfOp (\ x y -> Right $ x+y ) e1 e2
                Minus e1 e2  -> perfOp (\ x y -> Right $ x-y ) e1 e2
                Mult e1 e2   -> perfOp (\ x y -> Right $ x*y ) e1 e2
                Div e1 e2    -> perfOp (\ x y -> if y /= 0 then Right (x/y) else Left (DivisionByZero e1 e2)) e1 e2
                Pow e1 e2   -> perfOp (\ x y -> Right $ x** y ) e1 e2
                SquareRoot e1 -> perfOp (\_ v -> if v >=0 then Right (sqrt v) else Left (RootOfNegative e1)) e1 e1
                Var v         -> case M.lookup v ls of 
                                        Just val -> Right val
                                        Nothing -> Left $ UndefinedVariable v
       where  perfOp op e1 e2 = case (eval e1 ls, eval e2 ls) of
                    (Right v1, Right v2) -> op v1 v2
                    (Left er, _) -> Left er
                    (_, Left er) -> Left er


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
  trySimplify (Minus expr1 expr2) = if expr1 == expr2 then Const 0 else Minus (trySimplify expr1) (trySimplify expr2)

  trySimplify expr = expr