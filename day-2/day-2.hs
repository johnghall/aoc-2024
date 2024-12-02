import Debug.Trace

main = do
  contents <- getContents
  let rows = lines contents
  print . checkRows $ rows

checkRows :: [String] -> Integer
checkRows [] = 0
checkRows [x] = if checkRow . map readInt . words $ x then 1 else 0
checkRows (x : xs) = checkRows xs + checkRows [x]

readInt :: String -> Int
readInt = read

checkRow :: [Int] -> Bool
checkRow [] = True
checkRow [x, y] =
  let diff = abs (x - y)
   in diff >= 1 && diff <= 3
checkRow [x, y, z] =
  let diff_1 = x - y
      diff_2 = y - z
   in checkRow [x, y] && checkRow [y, z] && abs diff_1 + abs diff_2 == abs (diff_1 + diff_2)
checkRow (x : y : z : xs) = checkRow [x, y, z] && checkRow ([y, z] ++ xs)
