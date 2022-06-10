module Parser where

import Data.Char
import Control.Applicative

newtype Parser a = Parser (String -> Maybe (String, a))

instance Functor Parser where 
  fmap f p = 
    Parser $ \input -> do
      (rest, res) <- runParser input p
      return (rest, f res)

instance Applicative Parser where
  pure x = Parser $ \input -> return (input, x)
  fx <*> x = 
    Parser $ \input -> do
      (rest, fx) <- runParser input fx
      (rest', x) <- runParser rest x
      return (rest', fx x)

instance Alternative Parser where
  empty = Parser $ \input -> Nothing
  x <|> y = 
    Parser $ \input -> do
      case runParser input x of
        Nothing -> runParser input y
        (Just x) -> return x

instance Monad Parser where
  return = pure
  p >>= f =
    Parser $ \input -> do
      (rest, x) <- runParser input p
      runParser rest $ f x

runParser :: String -> Parser a -> Maybe (String, a)
runParser input (Parser f) = f input

item :: Parser Char
item = Parser $ \input ->
  case input of
    [] -> Nothing
    (x:xs) -> return (xs, x)

none :: Parser ()
none = Parser $ \input ->
  case runParser input item of
    Nothing -> return ("", ())
    _ -> empty

satisfy :: (Char -> Bool) -> Parser Char
satisfy pred = do
  x <- item
  if pred x then
            return x
            else
            empty

char :: Char -> Parser Char
char x = satisfy (==x)

digit :: Parser Char
digit = satisfy isDigit

number :: Parser Integer
number = read <$> some digit

spaces :: Parser String
spaces = many $ char ' '

string :: String -> Parser String
string [] = return []
string (x:xs) = do
  char x
  string xs
  return (x:xs)
