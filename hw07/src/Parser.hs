{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use lambda-case" #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}
{-# HLINT ignore "Evaluate" #-}
module Parser(parseExpression, runParser, Parser(..)) where

import Data.Char ( isAlpha, isAlphaNum, isDigit, digitToInt )
import Data.Map (fromList, member, Map, (!))
import Expr (BinaryOperator(..), UnaryOperator(..), Expr(..))
import Control.Applicative ( Alternative((<|>), empty, many) )
import GHC.Unicode (isSpace)


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


-- Checks if the next character is a space or the end of the input
satisfyEndOrWhiteSpace :: Parser ()
satisfyEndOrWhiteSpace = Parser $ \str ->
  case str of
    "" -> Right (str, ()) -- End of the string
    (c:_) | isSpace c -> Right (str, ()) -- Next char is whitespace
    _ -> Left "Expected space or end of input"

parseIdent :: Parser String
parseIdent = do
    h <- satisfy isAlpha
    t <- go
    satisfyEndOrWhiteSpace
    let ident = h : t
    if member ident unaryOperators then empty else return ident
  where
    go = (do
        x <- satisfy isAlphaNum
        y <- go
        return (x : y))
      <|>
        return []

binaryOperators :: Map Char BinaryOperator
binaryOperators = fromList [('+', Plus), ('-', Minus), ('*', Multiply), ('/', Divide), ('^', Power)]

parseBinOp :: Parser (Expr Int)
parseBinOp = do
  op <- satisfy (`member` binaryOperators)
  satisfy (== ' ')
  f <- parseExpression
  satisfy (== ' ')
  BinOp (binaryOperators ! op) f <$> parseExpression

unaryOperators :: Map String UnaryOperator
unaryOperators = fromList [("sqrt", Square)]

satisfyUnOp :: Parser String
satisfyUnOp = Parser $ \str ->
  go str ""
  where
    go rest@(' ':_) buff =
      if member buff unaryOperators
        then Right (rest, buff) else Left ("Unknown unary operation: " ++ buff)
    go (h:t) buff = go t (buff ++ [h])
    go [] _ = Left "Unary operator can't be empty"

parseUnOp :: Parser (Expr Int)
parseUnOp = do
  str <- satisfyUnOp
  satisfy (== ' ')
  UnOp (unaryOperators ! str) <$> parseExpression

parseInt :: Parser Int
parseInt = do
  h <- satisfy isDigit
  t <- go
  satisfyEndOrWhiteSpace
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
