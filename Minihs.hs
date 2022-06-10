import Parser
import Data.Char
import Control.Applicative
import System.IO

identifier :: Parser Ident
identifier = do
  x <- satisfy isLower
  xs <- some $ satisfy (\x -> isLower x || isUpper x)
  return (x:xs)
  <|> do
    x <- satisfy isLower
    return $ x:[]

type Ident = String

data Expr = Num Integer
          | Var Ident
          | Add Expr Expr
          | Sub Expr Expr
          | Mul Expr Expr
          | Div Expr Expr
          | Let Ident Expr Expr
          deriving Show

parseAdd :: Parser Expr
parseAdd = do
  spaces
  left <- parseTerm
  spaces
  char '+'
  spaces
  right <- parseExpr
  return (Add left right)

parseSub :: Parser Expr
parseSub = do
  spaces
  left <- parseTerm
  spaces
  char '-'
  spaces
  right <- parseExpr
  return (Sub left right)

parseMul :: Parser Expr
parseMul = do
  spaces
  left <- parseNumber <|> parseIdent
  spaces
  char '*'
  spaces
  right <- parseExpr
  return (Mul left right)

parseDiv :: Parser Expr
parseDiv = do
  spaces
  left <- parseNumber <|> parseIdent
  spaces
  char '/'
  spaces
  right <- parseExpr
  return (Div left right)

parseLet :: Parser Expr
parseLet = do
  spaces
  kw <- string "let"
  spaces
  id <- identifier
  spaces
  out <- parseExpr
  spaces
  string "in"
  spaces
  inn <- parseExpr
  return (Let id out inn)

parseNumber :: Parser Expr
parseNumber = do
  spaces
  num <- number
  return (Num num)

parseIdent :: Parser Expr
parseIdent = do
  spaces
  id <- identifier
  return (Var id)

parseExpr :: Parser Expr
parseExpr = parseLet <|> parseAdd <|> parseSub <|> parseTerm

parseTerm :: Parser Expr
parseTerm = parseMul <|> parseDiv <|> parseNumber <|> parseIdent

-- Evaluator

type Env = [(Ident, Integer)]

evaluate :: Expr -> Env -> Maybe (Env, Integer)
evaluate (Num x) env = return (env, x)
evaluate (Var id) env = do
  val <- findEnv id env
  return (env, val)
evaluate (Add l r) env = do
  (env', l') <- evaluate l env
  (env'', r') <- evaluate r env
  return (env'', l' + r')
evaluate (Sub l r) env = do
  (env', l') <- evaluate l env
  (env'', r') <- evaluate r env
  return (env'', l' - r')
evaluate (Mul l r) env = do
  (env', l') <- evaluate l env
  (env'', r') <- evaluate r env
  return (env'', l' * r')
evaluate (Div l r) env = do
  (env', l') <- evaluate l env
  (env'', r') <- evaluate r env
  return (env'', l' `div` r')
evaluate (Let id out inn) env = do
  (env', out') <- evaluate out env
  (env'', inn') <- evaluate inn $ appendEnv id out' env'
  return (env'', inn')

findEnv :: Ident -> Env -> Maybe Integer
findEnv id env = foldr (\(id', x) acc -> if id' == id then Just x else acc) Nothing env

appendEnv :: Ident -> Integer -> Env -> Env
appendEnv id val env = (id, val):env

evaluateMiniHs :: String -> Parser Expr -> Maybe Integer
evaluateMiniHs xs p = do
  (rest, val) <- runParser xs p
  (env, res) <- evaluate val []
  return res

main :: IO ()
main = do
  putStr "minihs>> " 
  hFlush stdout
  line <- getLine
  res <- return $ evaluateMiniHs line parseExpr
  case res of
    Nothing -> do
      putStrLn "There is something wrong with ur program bro"
      main
    (Just x) -> do
      putStrLn $ show x
      main
