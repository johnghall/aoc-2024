module Main where

import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char

type Parser = Parsec Void String

mulParser :: Parser Integer
mulParser = do
  _ <- many (noneOf "mul()")
  _ <- string "mul("
  firstNum <- some digitChar
  _ <- char ','
  secondNum <- some digitChar
  _ <- char ')'
  return $ read firstNum * read secondNum

multiMulParser :: Parser [Integer]
multiMulParser = many (try mulParser <|> (anySingle >> return 0))

main :: IO ()
main = do
  partOne

partOne :: IO ()
partOne = do
  input <- getContents
  putStrLn "Input text:"
  putStrLn input
  case parse multiMulParser "input" input of
    Left err -> do
      putStrLn "Parse error:"
      putStrLn $ errorBundlePretty err
    Right result -> do
      putStrLn "\nMatched mul() patterns:"
      if null result
        then putStrLn "No patterns found"
        else print (sum result)
