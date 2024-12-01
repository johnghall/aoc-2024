import Control.Monad
import Data.List (sort)
import System.IO

main = do
  contents <- getContents
  let numbers = map readInt . words $ contents
      list_1 = sort . first $ numbers
      list_2 = sort . second $ numbers
      diff = zipWith (\x y -> abs (x - y)) list_1 list_2
  print . sum $ diff

readInt :: String -> Int
readInt = read

first [] = []
first (x : xs) = x : second xs

second [] = []
second (x : xs) = first xs
