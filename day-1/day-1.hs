import Data.List (sort)
import System.IO

main = do
  contents <- getContents
  let numbers = map readInt . words $ contents
      list_1 = sort . first $ numbers
      list_2 = sort . second $ numbers
      diff = zipWith (\x y -> abs (x - y)) list_1 list_2
  print "Distance between lists:"
  print . sum $ diff
  print "Similarity:"
  print . countSimilarity list_1 $ list_2

readInt :: String -> Int
readInt = read

first [] = []
first (x : xs) = x : second xs

second [] = []
second (x : xs) = first xs

countAppearances :: (Integral a) => a -> [a] -> a
countAppearances _ [] = 0
countAppearances n (x : xs)
  | x == n = countAppearances n xs + 1
  | otherwise = countAppearances n xs

countSimilarity :: (Integral a) => [a] -> [a] -> a
countSimilarity [] _ = 0
countSimilarity (x : xs) y = countAppearances x y * x + countSimilarity xs y
