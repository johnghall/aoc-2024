main = do
  contents <- getContents
  let rows = lines contents
  print . checkRows $ rows
  print . checkRowsWithDampener $ rows

checkRows :: [String] -> Integer
checkRows [] = 0
checkRows [x] = if checkRow . map readInt . words $ x then 1 else 0
checkRows (x : xs) = checkRows xs + checkRows [x]

checkRowsWithDampener :: [String] -> Integer
checkRowsWithDampener [] = 0
checkRowsWithDampener [x] = if checkRowWithDampener . words $ x then 1 else 0
checkRowsWithDampener (x : xs) = checkRowsWithDampener xs + checkRowsWithDampener [x]

readInt :: String -> Int
readInt = read

checkRow :: [Int] -> Bool
checkRow [] = True
checkRow [x] = True
checkRow [x, y] =
  let diff = abs (x - y)
   in diff >= 1 && diff <= 3
checkRow [x, y, z] =
  let diff_1 = x - y
      diff_2 = y - z
   in checkRow [x, y] && checkRow [y, z] && abs diff_1 + abs diff_2 == abs (diff_1 + diff_2)
checkRow (x : y : z : xs) = checkRow [x, y, z] && checkRow ([y, z] ++ xs)

checkRowWithDampener :: [String] -> Bool
checkRowWithDampener [] = True
checkRowWithDampener x = (checkRows . map unwords $ removeOneEach x) >= 1

removeOneEach :: [a] -> [[a]]
removeOneEach [] = []
removeOneEach (x : xs) = xs : map (x :) (removeOneEach xs)
