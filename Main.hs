module Main where

import Text.Printf (printf)
import Control.Monad (unless)

data Either' a b  = Left' a | Right' b deriving Eq

instance Functor (Either' a) where
--  fmap :: (b -> b') -> Either' a b -> Either' a b'
  fmap f (Left' x) = Left' x
  fmap f (Right' y) = Right' $ f y

-- Proof:
-- fmap id (Left' x) = Left' x
-- fmap id (Right' y) = Right' $ id y = Right' y
-- fmap (f . g) (Left' x) = Left' x
-- fmap (f . g) (Right' y) = Right $ (f . g) y
-- fmap f (fmap g (Left' x)) = fmap f (Left' x) = Left' x
-- fmap f (fmap g (Right' y)) = fmap f (Right' $ g y) = Right' $ f (g y)

newtype Arrow a b = Arrow { wrapped :: (a -> b) }

instance Functor (Arrow a) where
  fmap f g = Arrow (f . wrapped g)

-- Proof:
-- fmap id (Arrow func) = Arrow ( id . wrapped func) = Arrow (wrapped func) = func (well, == won't work but on any argument they produce equal results)
-- fmap (f . g) (Arrow func) = Arrow (f . g . func)
-- fmap f $ fmap g (Arrow func) = fmap f $ Arrow (g . func) = Arrow (f . g . func)




addParentheses :: String -> String
addParentheses str = "(" ++ str ++ ")"

joinByOp :: Char -> String -> String -> String
joinByOp op str1 str2 = str1 ++ ' ':op:" " ++ str2

data Expr a = Sub (Expr a) (Expr a)
            | Add (Expr a) (Expr a)
            | Mul (Expr a) (Expr a)
            | Div (Expr a) (Expr a)
            | Pow (Expr a) (Expr a)
            | Lit a
            | Var String
            | Sqr (Expr a) deriving Eq

data Context = Pure
             | AddArg
             | SubLeftArg
             | SubRightArg
             | MulArg
             | DivLeftArg
             | DivRightArg
             | PowRightArg
             | PowLeftArg
             deriving (Eq, Ord)


showPure :: Show t => Expr t -> String
showPure (Add a b) = joinByOp '+' (showInCtx AddArg a) (showInCtx AddArg b)
showPure (Sub a b) = joinByOp '-' (showInCtx SubLeftArg a) (showInCtx SubRightArg b)
showPure (Mul a b) = joinByOp '*' (showInCtx MulArg a) (showInCtx MulArg b)
showPure (Div a b) = joinByOp '/' (showInCtx DivLeftArg a) (showInCtx DivRightArg b)
showPure (Pow a b) = joinByOp '^' (showInCtx PowLeftArg a) (showInCtx PowRightArg b)
showPure (Lit x) = show x
showPure (Var x) = '$':x
showPure (Sqr a) = "sqrt(" ++ showPure a ++ ")"

showInCtx :: Show t => Context -> Expr t -> String
showInCtx ctx expr = let
                       pure = showPure expr
                       inParentheses = addParentheses pure
                       delimitByCtx context = if ctx <= context then pure else inParentheses
                     in case expr of
  Lit _ -> pure
  Sqr _ -> pure
  Var _ -> pure
  Pow _ _ -> delimitByCtx PowRightArg
  Div _ _ -> delimitByCtx DivLeftArg
  Mul _ _ -> delimitByCtx DivLeftArg
  Sub _ _ -> delimitByCtx SubLeftArg
  Add _ _ -> delimitByCtx SubLeftArg


instance Show t => Show (Expr t) where
  show = showPure

type VarList t = [(String, t)]

showVarList :: Show t => VarList t -> String
showVarList [] = ""
showVarList xs = show $ map (\entry -> show (fst entry) ++ "=" ++ show (snd entry)) xs

data Error t = NegativeInSqrt (VarList t) (Expr t)
             | NegativeInPower (VarList t) (Expr t)
             | ZeroInDivision (VarList t) (Expr t)
             | VariableNotFound String deriving Eq

showEvalResult :: (Floating t, Show t, Ord t) => Either (Error t) t -> String
showEvalResult (Right a) = show a
showEvalResult (Left err) = show err

instance (Floating t, Show t, Ord t) => Show (Error t) where
  show err = "Evaluation error! " ++ case err of
      NegativeInSqrt vl e -> "Argument of sqrt " ++ showFullExpr vl e ++ " evaluated to " ++ showEvalResult (eval vl e)
      NegativeInPower vl e -> "Base of exponentiation " ++ showFullExpr vl e ++ " evaluated to " ++ showEvalResult (eval vl e)
      ZeroInDivision vl e -> "Divisor " ++ showFullExpr vl e ++ " is evaluated to 0"
      VariableNotFound var -> "Variable '" ++ var ++ "' is not assigned a value!"
    where showFullExpr varList expr = "'" ++ showVarList varList ++ show expr ++ "'"

type EvalResult t = Either (Error t) t

eval :: (Floating t, Ord t) => [(String, t)] -> Expr t -> EvalResult t
eval varList expr = case expr of
  Lit x -> Right x
  Var s -> lookupVar s
  Add a b -> evalPureBinary (+) a b
  Sub a b -> evalPureBinary (-) a b
  Mul a b -> evalPureBinary (*) a b
  Div a b -> case (eval' a, eval' b) of
    (Right _, Right 0.0) -> Left (ZeroInDivision varList b)
    (Right evalA, Right evalB) -> Right (evalA / evalB)
    errorEvaluation -> extractError errorEvaluation
  Pow a b -> case (eval' a, eval' b) of
    (Right evalA, Right evalB) | evalA <= 0.0 -> Left (NegativeInPower varList a)
                               | otherwise -> Right (evalA ** evalB)
    errorEvaluation -> extractError errorEvaluation
  Sqr a -> case eval' a of
    Right evalA | evalA < 0.0 -> Left (NegativeInSqrt varList a)
                | otherwise -> Right (evalA ** 0.5)
    errorEvaluation -> errorEvaluation
  where
    eval' = eval varList
    extractError :: (EvalResult t, EvalResult t) -> EvalResult t
    extractError (Left err, _) = Left err
    extractError (_, Left err) = Left err
    evalPureBinary op a b = case (eval' a, eval' b) of
      (Right evalA, Right evalB) -> Right (op evalA evalB)
      errorEvaluation -> extractError errorEvaluation
    lookupVar s = case lookup s varList of
      Nothing -> Left (VariableNotFound s)
      Just x -> Right x

instance Num t => Num (Expr t) where
  (+) expr1 expr2 = Add expr1 expr2
  (-) expr1 expr2 = Sub expr1 expr2
  (*) expr1 expr2 = Mul expr1 expr2
  fromInteger i = Lit $ fromInteger i
  abs expr = expr
  signum expr = Lit 1


applyToExpr :: (Expr t -> (Expr t, Bool)) -> Expr t -> (Expr t, Bool)
applyToExpr applyFunc expr = case expr of
    Lit x -> (Lit x, False)
    Var x -> (Var x, False)
    Add a b -> subtreeFunc Add a b
    Sub a b -> subtreeFunc Sub a b
    Mul a b -> subtreeFunc Mul a b
    Div a b -> subtreeFunc Div a b
    Pow a b -> subtreeFunc Div a b
    Sqr a -> case applyFunc a of
      (res, bool) -> (Sqr res, bool)
  where subtreeFunc joinFunc subtreeA subtreeB = case (applyFunc subtreeA, applyFunc subtreeB) of
          ((res1, bool1), (res2, bool2)) -> (joinFunc res1 res2, bool1 || bool2)

eliminate :: (Num t, Eq t) => Expr t -> (Expr t, Bool)
eliminate e = case e of
  Add (Lit 0) e -> (e, True)
  Add e (Lit 0) -> (e, True)
  Sub e (Lit 0) -> (e, True)
  Mul (Lit 1) e -> (e, True)
  Mul e (Lit 1) -> (e, True)
  Div e (Lit 1) -> (e, True)
  Mul (Lit 0) e -> (Lit 0, True)
  Mul e (Lit 0) -> (Lit 0, True)
  Div (Lit 0) e -> (Lit 0, True)
  Pow e (Lit 0) -> (Lit 1, True)
  Pow e (Lit 1) -> (e, True)
  Pow (Lit 1) e -> (Lit 1, True)
  Sub a b | a == b -> (Lit 0, True)
  Div a b | a == b -> (Lit 1, True)
  e -> applyToExpr eliminate e

simplify :: (Num t, Eq t) => Expr t -> Expr t
simplify e = case eliminate e of
  (result, False) -> result
  (result, True) -> simplify result

casesShow :: [(Expr Int, String)]
casesShow = [ (Var "x", "$x"), (Lit 5, "5"), (5 + 5, "5 + 5"), (5 * 5, "5 * 5")
            , (Div 5 5, "5 / 5"), (5 - 5, "5 - 5"), (Pow 5 5, "5 ^ 5"), (Var "x" * 5, "$x * 5")
            , (Sqr 5, "sqrt(5)"), ((5 + 5) * (5 - 5), "(5 + 5) * (5 - 5)")
            , (Pow (5 + 5) (5 * 5), "(5 + 5) ^ (5 * 5)"), (5 * 5 + 5, "5 * 5 + 5")
            , (Div (Pow (5 * 5 + 5) 5) 5, "(5 * 5 + 5) ^ 5 / 5")
            ]

casesSimplify :: [(Expr Double, Expr Double)]
casesSimplify = [ (Var "x" * 0, 0)
                , (Var "x" * 1, Var "x")
                , (Var "x", Var "x")
                , (5 * 0 + 4 * 1, 4)
                , (Var "x" - Pow (Var "x") 1 + 10, 10)
                , (Pow (Var "z") 0 - Pow 1 (Var "z"), 0)
                , (Div (Var "x") (Div (Var "x") 1), 1)
                , (0 + Var "x", Var "x")
                , (Var "x" - 0, Var "x")
                , (1 * (Var "x" - 4), Var "x" - 4)
                ]

casesDouble :: [((Expr Double, VarList Double), EvalResult Double)]
casesDouble = [ ((four, []), Right 4.0), ((sum, []), Right 9.0), ((diff, []), Right (-1.0))
              , ((four * five, []), Right 20.0), ((Div four five, []), Right 0.8)
              , ((Pow four five, []), Right 1024.0), ((Sqr four, []), Right 2.0)
              , ((diff * diff, []), Right 1.0), ((Div sum diff, []), Right (-9.0))
              , ((Div diff zero, []), Left (ZeroInDivision [] zero))
              , ((Pow diff sum, []), Left (NegativeInPower [] diff))
              , ((Sqr diff, []), Left (NegativeInSqrt [] diff))
              , ((Pow zero sum, []), Left (NegativeInPower [] zero))
              , ((Var "x", xyz), Right 1.0)
              , ((Var "x" + Var "y", xyz), Right 3.0)
              , ((Var "x" * Var "a", xyz), Left $ VariableNotFound "a")
              ]
  where four = Lit 4
        five = Lit 5
        sum = four + five
        diff = four - five
        zero = four - four
        xyz = [("x", 1.0), ("y", 2.0), ("z", 3.0)]

testDouble :: (Expr Double, VarList Double) -> EvalResult Double -> IO ()
testDouble (expr, varList) expected =
    let actual = eval varList expr in
    unless (expected == actual) $ describeFailure actual
  where
    describeFailure actual =
      printf "eval (%s) should be %s but it was %s" (show expr) (show expected) (show actual)

testSimplify :: Expr Double -> Expr Double -> IO()
testSimplify expr expected =
    let actual = simplify expr in
    unless (expected == actual) $ describeFailure actual
  where
    describeFailure actual =
      printf "simplify (%s) should be %s but it was %s" (show expr) (show expected) (show actual)

testShow :: Expr Int -> String -> IO ()
testShow expr expected =
    let actual = show expr in
    unless (expected == actual) $ describeFailure actual
  where
    describeFailure actual =
      printf "show (%s) should be %s bui it was %s" (show expr) expected actual

main :: IO ()
main = do
  mapM_ (uncurry testDouble) casesDouble
  mapM_ (uncurry testSimplify) casesSimplify
  mapM_ (uncurry testShow) casesShow
