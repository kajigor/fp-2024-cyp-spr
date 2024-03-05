module Main where 

import Text.Printf (printf)
import Control.Monad (unless)
import Data.Either (isRight, fromRight)
import Data.Text.Internal.Read (IParser(P))
import Data.Maybe (fromJust, isNothing)
import Data.List (find)
import GHC.Exts.Heap (GenClosure(value))


newtype Either2 a b = Either2 (Either a b) deriving (Show)

instance Functor (Either2 a) where
    fmap :: (b -> c) -> Either2 a b -> Either2 a c
    fmap f (Either2 (Left x)) = Either2(Left x)
    fmap f (Either2 (Right x)) = Either2(Right (f x))

{-

  1) fmap id = id

  fmap id (Either2 (Left x)) = Either2(Left x)
  fmap id (Either2 (Right x)) = Either2(Right (id x)) =  Either2(Right x)  


  2) fmap (f . g)  ==  fmap f . fmap g

  fmap (f . g) (Either2 (Left x)) = Either2 (Left x) = fmap f (Either2 (Left x)) = fmap f . fmap g (Either2 (Left x))
  fmap (f . g) (Either2 (Right x)) = Either2 (Right (f.g) x) = fmap f (Either2 (Right g x)) = fmap f . fmap g (Either2 (Right x))

-}

newtype MyArrow a b = MyArrow((->) a b)

instance Functor (MyArrow f) where
    fmap :: (a -> b) -> MyArrow f a -> MyArrow f b
    fmap f (MyArrow g)  = MyArrow (f . g)

{-
  1) fmap id = id
  
  h has type t -> a
  f has type a -> b
  g has type b -> c

  fmap id h = id . h = h 


  2) fmap (f . g)  ==  fmap f . fmap g

  fmap (f . g) h =  f . g . h = fmap f (g . h) = fmap f (fmap g h)

-}

data Expr a = Var String
              | Const a
              | Plus (Expr a) (Expr a)
              | Minus (Expr a) (Expr a)
              | Mult (Expr a) (Expr a)
              | Div (Expr a) (Expr a)
              | Pow (Expr a) (Expr a)
              | Root (Expr a)
              deriving (Eq)


instance (Show a) => Show (Expr a) where 
  show (Var ex) = ex
  show (Const ex) = show ex
  show (Plus ex1 ex2) = "(" ++ show ex1 ++ " + " ++ show ex2 ++ ")"
  show (Minus ex1 ex2) = "(" ++ show ex1 ++ " - " ++ show ex2 ++ ")"
  show (Mult ex1 ex2) = "(" ++ show ex1 ++ " * " ++ show ex2 ++ ")"
  show (Div ex1 ex2) = "(" ++ show ex1 ++ " / " ++ show ex2 ++ ")"
  show (Pow ex1 ex2) = "(" ++ show ex1 ++ " ** " ++ show ex2 ++ ")" 
  show (Root ex) = "(" ++ "root " ++ show ex ++ ")"

data Error = DivideByZero String
            | NegativeFromRoot String 
            | PowNaN String 
            | VariableNotApplied String
            | ComplexError Error Error
            deriving (Eq)

instance Show Error where 
  show (DivideByZero msg) = "DivideByZero error " ++ show msg
  show (NegativeFromRoot msg) = "NegativeFromRoot error " ++ show msg
  show (PowNaN msg) = "PowNaN error " ++ show msg
  show (VariableNotApplied msg) = "VariableNotApplied error " ++ show msg
  show (ComplexError error1 error2) = "ComplexErrorOf " ++ show error1 ++ " and " ++ show error2

andExprError :: (Expr a -> Expr a -> Expr a) -> Either Error (Expr a) -> Either Error (Expr a) -> Either Error (Expr a) -- priority for the errors, operands otherwise
andExprError _ (Left error1) (Left error2) = Left (ComplexError error1 error2)
andExprError _ (Right expr1) (Left error2) = Left error2
andExprError _ (Left error1) (Right expr2) = Left error1
andExprError oper (Right expr1) (Right expr2) = Right (oper expr1 expr2)


resolve :: (RealFloat a, Ord a, Show a) => Either Error (Expr a) -> [(String, a)] -> Either Error (Expr a)
resolve (Left error) _ = Left error

resolve (Right (Const n)) _ = Right (Const n)

resolve (Right (Root (Const n))) _
  | n < 0 = Left (NegativeFromRoot ("(root of " ++ show n ++ ")"))
  | otherwise = Right (Const (sqrt n))

resolve (Right (Plus (Const n1) (Const n2))) _ = Right $ Const (n1 + n2)
resolve (Right (Minus (Const n1) (Const n2))) _ = Right $ Const (n1 - n2)
resolve (Right (Mult (Const n1) (Const n2))) _ = Right $ Const (n1 * n2)
resolve (Right (Div (Const n1) (Const n2))) _ 
  | n2 == 0 = Left (DivideByZero ("(div of " ++ show n1 ++ " and " ++ show n2 ++ ")"))
  | otherwise = Right $ Const (n1 / n2)

resolve (Right (Pow (Const n1) (Const n2))) _
  | isNaN (n1 ** n2)= Left (PowNaN ("(pow of " ++ show n1 ++ " and " ++ show n2 ++ ")"))
  | otherwise = Right (Const (n1 ** n2))

resolve (Right (Var v)) vars
  | isNothing (lookup v vars) = Left (VariableNotApplied ("(variable " ++ v ++ ")"))
  | otherwise = Right $ Const (fromJust $ lookup v vars)

resolve (Right (Root expr)) vars = resolve (Root <$> resolve (Right expr) vars) vars -- propagate error and recursive Root if correct
resolve (Right (Plus expr1 expr2)) vars = resolve (andExprError Plus (resolve (Right expr1) vars) (resolve (Right expr2) vars)) vars
resolve (Right (Minus expr1 expr2)) vars = resolve (andExprError Minus (resolve (Right expr1) vars) (resolve (Right expr2) vars)) vars
resolve (Right (Mult expr1 expr2)) vars = resolve (andExprError Mult (resolve (Right expr1) vars) (resolve (Right expr2) vars)) vars
resolve (Right (Div expr1 expr2)) vars = resolve (andExprError Div (resolve (Right expr1) vars) (resolve (Right expr2) vars)) vars
resolve (Right (Pow expr1 expr2)) vars = resolve (andExprError Pow (resolve (Right expr1) vars) (resolve (Right expr2) vars)) vars


exprToNum :: Expr a -> a
exprToNum (Const n) = n

eval :: (RealFloat a, Ord a, Show a) => Expr a -> [(String, a)] -> Either Error a
eval expr vars = exprToNum <$> resolve (Right expr) vars 

simplify :: (RealFloat a, Ord a, Show a) => Expr a -> Expr a

simplify (Const x) = Const x
simplify (Var x) = Var x

simplify (Plus (Const 0) expr) = simplify expr
simplify (Plus expr (Const 0)) = simplify expr
simplify (Plus (Const a) (Const b)) = if a + b == 0 then Const 0 else Minus (simplify (Const a)) (simplify (Const b))


simplify (Minus (Const 0) expr) = simplify expr
simplify (Minus expr (Const 0)) = simplify expr
simplify (Minus (Const a) (Const b)) = if a - b == 0 then Const 0 else Minus (simplify (Const a)) (simplify (Const b))


simplify (Mult (Const 0) expr) = Const 0
simplify (Mult expr (Const 0)) = Const 0

simplify (Mult (Const 1) expr) = simplify expr
simplify (Mult expr (Const 1)) = simplify expr

simplify (Div expr (Const 1)) = simplify expr
simplify (Div (Const 0) expr) = if simplify expr /= Const 0 then Const 0 else Div (Const 0) (Const 0)

simplify (Root (Const 1)) = Const 1
simplify (Root (Const 0)) = Const 0

simplify (Root expr) = Root $ simplify expr
simplify (Plus expr1 expr2) = Plus (simplify expr1) (simplify expr2) -- I wonder if there is a prettier way
simplify (Minus expr1 expr2) = Minus (simplify expr1) (simplify expr2) 
simplify (Mult expr1 expr2) = Mult (simplify expr1) (simplify expr2) 
simplify (Div expr1 expr2) = if (simple1 == simple2) && (simple1 /= Const 0) && (simple2 /= Const 0)
                             then Const 1 else Div simple1 simple2
                              where simple1 = simplify expr1
                                    simple2 = simplify expr2
simplify (Pow expr1 expr2) = Pow (simplify expr1) (simplify expr2)

cases :: (RealFloat a, Ord a, Show a) => [(Expr a, [(String, a)], Either Error Double)]
cases = [
    (Root $ Const 4, [], Right 2.0),
    (Plus (Root (Const 9)) (Minus (Const 2) (Const 7)), [], Right (-2.0)),
    (Div (Pow (Const 9) (Const 3)) (Pow (Const 4) (Const 2)), [], Right 45.5625),
    (Mult (Div (Const 3) (Const 150)) (Div (Const 200) (Const 10)), [], Right 0.4),
    -- (1 - 1^3/3!) * 6
    (Mult (Const 6) (Minus (Const 1) (Div (Pow (Const 1) (Const 3)) (Mult (Const 2) (Const 3)))), [], Right 5.0),
    (Root (Const (-1)), [], Left $ NegativeFromRoot "(root of -1.0)"),
    (Div (Mult (Const 7.0) (Const 2.0)) (Minus (Const 4.0) (Const 4.0)), [], Left $ DivideByZero "(div of 14.0 and 0.0)"),
    (Div (Pow (Const (-1.0)) (Const 0.5)) (Root (Const 4.0)), [], Left $ PowNaN "(pow of -1.0 and 0.5)"),
    (Plus (Root (Const (-100500))) (Div (Const 100) (Const 0)), [], Left $ ComplexError (NegativeFromRoot "(root of -100500.0)") (DivideByZero "(div of 100.0 and 0.0)")),
  
    (Var "A", [("A", 1)], Right 1),
    (Div (Var "A") (Var "B"), [("A", 1), ("B", 0)], Left  $ DivideByZero "(div of 1.0 and 0.0)"),
    (Var "A", [], Left $ VariableNotApplied "(variable A)"),
    (Var "A", [("A", 1), ("B", 2)], Right 1.0), -- priority to the first occurrence

    (Mult (Const 6) (Minus (Var "A") (Div (Pow (Var "A") (Var "C")) (Mult (Var "B") (Var "C")))), [("A", 1), ("B", 2), ("C", 3)], Right 5.0)
  ]


simplifyCases :: (RealFloat a, Ord a, Show a) => [(Expr a, Expr a)]
simplifyCases = [
  (Plus (Const 1.0) (Const 0.0), Const 1.0),
  (Plus (Const 1.0) (Const (-1.0)), Const 0.0),
  (Plus (Const 0.0) (Const 2.0), Const 2.0),
  (Plus (Plus (Const 0.0) (Plus (Const 0.0) (Const 4.0))) (Const 0.0), Const 4.0),
  (Plus (Var "x") (Const 0.0), Var "x"),
  (Plus (Var "x") (Var "y"), Plus (Var "x") (Var "y")),
  (Minus (Const 3.0) (Const 0.0), Const 3.0),
  (Minus (Const 3.0) (Const 3.0), Const 0.0),
  (Minus (Const 3.0) (Const 4.0), Minus (Const 3.0) (Const 4.0)),
  (Minus (Var "x") (Var "y"), Minus (Var "x") (Var "y")),
  (Mult (Const 2.0) (Const 1.0), Const 2.0),
  (Mult (Const 1.0) (Const 2.0), Const 2.0),
  (Mult (Const 2.0) (Const 0.0), Const 0.0),
  (Mult (Const 0.0) (Const 2.0), Const 0.0),
  (Mult (Var "x") (Const 0.0), Const 0.0),
  (Mult (Const 0.0) (Var "x"), Const 0.0),
  (Div (Const 0.0) (Const 5.0), Const 0.0),
  (Div (Const 5.0) (Const 0.0), Div (Const 5.0) (Const 0.0)),
  (Div (Const 4.0) (Const 4.0), Const 1.0),
  (Div (Const 5.0) (Const 1.0), Const 5.0),
  (Div (Var "x") (Const 1.0), Var "x"),
  (Div (Var "x") (Var "x"), Const 1.0)
  ]

test :: (RealFloat a, Ord a, Show a) => Expr a -> [(String, a)] -> Either Error a -> IO () 
test expr vars expected = 
    let actual = eval expr vars in 
    unless (expected == actual) $ describeFailure actual
  where 
    describeFailure actual = 
      printf "eval (%s) should be %s but it was %s\n" (show expr) (show expected) (show actual) 
  
testSimplify :: (RealFloat a, Ord a, Show a) => Expr a -> Expr a -> IO ()
testSimplify expr expected =
    let actual = simplify expr in
    unless (expected == actual) $ describeFailure actual
  where
    describeFailure actual =
      printf "Simplify (%s) should be %s but it was %s\n" (show expr) (show expected) (show actual)

main :: IO () 
main = do 
  mapM_ (\(expr, expected, vars) -> test expr expected vars) cases
  mapM_ (uncurry testSimplify) simplifyCases
  putStrLn "Done"