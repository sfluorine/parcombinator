module Parser (module Parser, module Control.Applicative) where

-- Following Prof. Graham Hutton's parser combinator library
-- References:
--  https://youtu.be/dDtZLm7HIJs

import Control.Applicative

type Error = String

newtype Parser a = Parser (String -> Either Error (String, a))

-- Parser primitive

item :: Parser Char
item = Parser (\input ->
  case input of
    [] -> Left "Unexpected end of file"
    (x:xs) -> return (xs,x))

satisfy :: (Char -> Bool) -> Parser Char
satisfy p = do
  x <- item
  if p x then return x else Parser (\_ -> Left $ "Unexpected character: " ++ [x])

char :: Char -> Parser Char
char x = satisfy (==x)

space :: Parser ()
space = do
  many $ char ' '
  return ()

token :: Parser a -> Parser a
token p = do
  space
  p

-- Failing parser

parserFail :: String -> Parser a
parserFail error = Parser (\_ -> Left error)

-- Running parser

parse :: Parser a -> String -> Either Error (String, a)
parse (Parser p) input = p input

runParser :: Parser a -> String -> Either Error a
runParser p input = do
  (rest, val) <- parse p input
  return val

-- Sequencing parser

instance Functor Parser where
  fmap f p = Parser (\input ->
    case parse p input of
      (Left error) -> Left error
      (Right (r, v)) -> return (r, f v))

instance Applicative Parser where
  pure x = Parser (\input -> return (input, x))
  fp <*> p = Parser (\input ->
    case parse fp input of
      (Left error) -> Left error
      (Right (r, f)) -> parse (f <$> p) r)

instance Monad Parser where
  return = pure
  p >>= f = Parser (\input ->
    case parse p input of 
      (Left error) -> Left error
      (Right (r, x)) -> parse (f x) r)

instance Alternative Parser where
  empty = Parser (\input -> Left "empty")
  l <|> r = Parser (\input ->
    case parse l input of
      (Left error) -> parse r input
      (Right res) -> return res)
