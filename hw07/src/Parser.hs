{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use lambda-case" #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}
{-# HLINT ignore "Evaluate" #-}
module Parser where

import Data.Char ( isAlpha, isAlphaNum, isDigit, digitToInt )
import Control.Applicative ( Alternative((<|>), empty, many) )
import Expr (Expr(..), eval)
import Text.Parsec (parserPlus, digit)


-- It's not clear how to compose the parsers above, so people usually use a different abstraction for a parser. 
-- A parser consumes the prefix of an input String while it's a valid string of the language being parsed. 
-- Then the unread suffix is returned along with the result of the parsing. 
-- The result may be a string (for identifiers), an integer (for numbers), 
-- some algebraic data type for more complex langugaes (for example, Expr for expressions), 
-- or even a function. 
newtype Parser a
  = Parser { runParser :: String -> Either String (String, a)}

-- This abstraction of a parser is a Functor, which allows us to transform the parser's results. 
instance Functor Parser where
  fmap :: (a -> b) -> Parser a -> Parser b
  fmap f p = Parser $ \input ->
    case runParser p input of
      Left x -> Left x
      Right (suff, r) -> Right (suff, f r)

-- The parser is also an applicative functor, which simplifies composition.       
instance Applicative Parser where
  pure :: a -> Parser a
  pure res = Parser $ \str -> Right (str, res)

  (<*>) :: Parser (a -> b) -> Parser a -> Parser b
  (<*>) f p = Parser $ \str ->
    case runParser f str of
      Right (str', f') ->
        case runParser p str' of
          Right (str'', a) -> Right (str'', f' a)
          Left x -> Left x
      Left x -> Left x

-- Monadic bind is something which expresses the idea of sequential parser application. 
-- First parse the string with this parser, and then parse the rest with that parser.  
-- This is one of two most important operations on parsers.    
instance Monad Parser where
  (>>=) :: Parser a -> (a -> Parser b) -> Parser b
  (>>=) f p = Parser $ \str ->
    case runParser f str of
      Right (str', res) -> runParser (p res) str'
      Left x -> Left x

-- Alternative operation allows us to express that something is either this or that. 
-- Note that it favors the left-hand parser: if it succeeds consuming any prefix of the input string, 
-- the right parser will not be tried. 
instance Alternative Parser where
  empty :: Parser a
  empty = Parser $ const (Left "No strings in its language") -- a parser which always reports an error: no strings in its language.

  (<|>) :: Parser a -> Parser a -> Parser a
  (<|>) l r = Parser $ \str ->
    case runParser l str of
      Right (str', res) -> Right (str', res)
      Left x -> runParser r str


satisfy :: (Char -> Bool) -> Parser Char
satisfy p = Parser $ \str ->
  case str of
    (h:t) | p h -> Right (t, h)
    _ -> Left "Predicate is not satisfied"


parseIdent :: Parser String
parseIdent = do
    h <- satisfy isAlpha 
    t <- go              
    let ident = h : t    
    if ident `elem` unaryOps then empty else return ident
  where
    go = (do                    
        x <- satisfy isAlphaNum 
        y <- go                 
        return (x : y))         
      <|>
        return []               

parseBinOp :: Parser (Expr Int)
parseBinOp = do
  op <- satisfy pred
  satisfy (== ' ')
  f <- parseExpression
  satisfy (== ' ')
  s <- parseExpression
  return (case op of
      '+' -> Add f s
      '-' -> Subtract f s
      '*' -> Multiply f s
      '/' -> Divide f s
      '^' -> Power f s)
  where
    pred = (`elem` ['+', '-', '/', '*', '^'])

unaryOps :: [String]
unaryOps = ["sqrt"]

satisfyUnOp :: Parser String
satisfyUnOp :: Parser String = Parser $ \str ->
  go str ""
  where
    go rest@(' ':t) buff =
      if buff `elem` unaryOps
        then Right (rest, buff) else Left ("Unknown unary operation: " ++ buff)
    go (h:t) buff = go t (buff ++ [h])
    go [] _ = Left "Unary operator can't be empty"

parseUnOp :: Parser (Expr Int)
parseUnOp = do
  str <- satisfyUnOp
  satisfy (== ' ')
  expr <- parseExpression
  return (
    case str of
    "sqrt" -> Square expr
    )

parseInt :: Parser Int
parseInt = do
  h <- satisfy isDigit
  t <- go
  return (stoi (h:t))
  where
    go = (do
        x <- satisfy isDigit
        y <- go
        return (x : y))
      <|>
        return []
    stoi = foldl1 (\a x -> a * 10 + x) . map digitToInt

parseConst :: Parser (Expr Int)
parseConst = do
  Const <$> parseInt

parseVar :: Parser (Expr Int)
parseVar = do
  Var <$> parseIdent

parseExpression :: Parser (Expr Int)
parseExpression = do
  parseBinOp <|> parseUnOp <|> parseVar <|> parseConst
