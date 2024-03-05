module Main where

import Text.Printf (printf)
import Control.Monad (unless)


addParentheses :: String -> String
addParentheses str = "(" ++ str ++ ")"

joinByOp :: Char -> String -> String -> String
joinByOp op str1 str2 = str1 ++ ' ':op:" " ++ str2

data Expr = Sub Expr Expr
          | Add Expr Expr
          | Mul Expr Expr
          | Div Expr Expr
          | Pow Expr Expr
          | Lit Double
          | Sqr Expr deriving Eq

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


showPure :: Expr -> String
showPure (Add a b) = joinByOp '+' (showInCtx AddArg a) (showInCtx AddArg b)
showPure (Sub a b) = joinByOp '-' (showInCtx SubLeftArg a) (showInCtx SubRightArg b)
showPure (Mul a b) = joinByOp '*' (showInCtx MulArg a) (showInCtx MulArg b)
showPure (Div a b) = joinByOp '/' (showInCtx DivLeftArg a) (showInCtx DivRightArg b)
showPure (Pow a b) = joinByOp '^' (showInCtx PowLeftArg a) (showInCtx PowRightArg b)
showPure (Lit x) = show x
showPure (Sqr a) = "sqrt(" ++ showPure a ++ ")"

showInCtx :: Context -> Expr -> String
showInCtx ctx expr = let
                       pure = showPure expr
                       inParentheses = addParentheses pure
                       delimitByCtx context = if ctx <= context then pure else inParentheses
                     in case expr of
  Lit _ -> pure
  Sqr _ -> pure
  Pow _ _ -> delimitByCtx PowRightArg
  Div _ _ -> delimitByCtx DivLeftArg
  Mul _ _ -> delimitByCtx DivLeftArg
  Sub _ _ -> delimitByCtx SubLeftArg
  Add _ _ -> delimitByCtx SubLeftArg


instance Show Expr where
  show = showPure

data Error = NegativeInSqrt Expr | NegativeInPower Expr | ZeroInDivision Expr deriving Eq

showEvalResult :: Either Error Double -> String
showEvalResult (Right a) = show a
showEvalResult (Left err) = show err

instance Show Error where
  show (NegativeInSqrt expr) = "Evaluation error! Argument of sqrt '" ++ show expr ++ "' evaluated to " ++ showEvalResult (eval expr)
  show (NegativeInPower expr) = "Evaluation error! Base of exponentiation '" ++ show expr ++ "' evaluated to " ++ showEvalResult (eval expr)
  show (ZeroInDivision expr) = "Evaluation error! Divisor '" ++ show expr ++ "' is evaluated to 0"

eval :: Expr -> Either Error Double
eval expr = case expr of
  Lit x -> Right x
  Add a b ->  evalPureBinary (+) a b
  Sub a b -> evalPureBinary (-) a b
  Mul a b -> evalPureBinary (*) a b
  Div a b -> case (eval a, eval b) of
    (Right _, Right 0.0) -> Left (ZeroInDivision b)
    (Right evalA, Right evalB) -> Right (evalA / evalB)
    errorEvaluation -> extractError errorEvaluation
  Pow a b -> case (eval a, eval b) of
    (Right evalA, Right evalB) | evalA <= 0 -> Left (NegativeInPower a)
                               | otherwise -> Right (evalA ** evalB)
    errorEvaluation -> extractError errorEvaluation
  Sqr a -> case eval a of
    Right evalA | evalA < 0 -> Left (NegativeInSqrt a)
                | otherwise -> Right (evalA ** 0.5)
    errorEvaluation -> errorEvaluation
  where
    extractError :: (Either Error Double, Either Error Double) -> Either Error Double
    extractError (Left err, _) = Left err
    extractError (_, Left err) = Left err
    evalPureBinary :: (Double -> Double -> Double) -> Expr -> Expr -> Either Error Double
    evalPureBinary op a b = case (eval a, eval b) of
      (Right evalA, Right evalB) -> Right (op evalA evalB)
      errorEvaluation -> extractError errorEvaluation

cases :: [(Expr, Either Error Double)]
cases = [ (four, Right 4.0), (sum, Right 9.0), (diff, Right (-1.0))
        , (Mul four five, Right 20.0), (Div four five, Right 0.8)
        , (Pow four five, Right 1024.0), (Sqr four, Right 2.0)
        , (Mul diff diff, Right 1.0), (Div sum diff, Right (-9.0))
        , (Div diff zero, Left (ZeroInDivision zero))
        , (Pow diff sum, Left (NegativeInPower diff))
        , (Sqr diff, Left (NegativeInSqrt diff))
        , (Pow zero sum, Left (NegativeInPower zero)) ]
  where four = Lit 4
        five = Lit 5
        sum = Add four five
        diff = Sub four five
        zero = Sub four four

test :: Expr -> Either Error Double -> IO ()
test expr expected =
    let actual = eval expr in
    unless (expected == actual) $ describeFailure actual
  where
    describeFailure actual =
      printf "eval (%s) should be %s but it was %s" (show expr) (show expected) (show actual)

main :: IO ()
main = do
  mapM_ (uncurry test) cases
