- Example

```haskell
import Parser
import Data.Char

data Value = I Integer
           | F Float
           deriving Show

parseNumber :: Parser Value
parseNumber = F <$> parseFloat <|> I <$> parseInteger

parseFloat :: Parser Float
parseFloat = read <$> do
  l <- token (many $ satisfy isDigit) -- token is parsing zero or more space (' ') before many $ satisfy isDigit
  dot <- char '.'
  r <- many $ satisfy isDigit
  return $ l ++ [dot] ++ r

parseInteger :: Parser Integer
parseInteger = read <$> (many $ satisfy isDigit)

main :: IO ()
main = do
  f <- return $ runParser parseNumber "34.5"
  i <- return $ runParser parseNumber "34"
  case f of
    (Right (F f)) -> putStrLn $ "Float: " ++ show f
  case i of
    (Right (I i)) -> putStrLn $ "Integer: " ++ show i
```
