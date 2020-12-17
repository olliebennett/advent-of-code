import AoC
import Data.List

input = readDayInputLines 5

filterTupleBySecond :: Char -> (Int, Char) -> Bool
filterTupleBySecond char tup = snd tup == char

seatRow :: String -> Int
seatRow row_str = do
  let powers = [64, 32, 16, 8, 4, 2, 1]
  let zipped = zip powers row_str -- [(64, 'F'), (32, 'B'), ...]

  -- Filter out only those tuples with 'B' (as the 'F's count for nothing)
  let filtered = filter (filterTupleBySecond 'B') zipped

  -- Return the sum of the first (fst) elements in each included tuple
  sum (map fst filtered)

seatCol :: String -> Int
seatCol col_str = do
  let powers = [4, 2, 1]
  let zipped = zip powers col_str -- [(4, 'L'), (2, 'R'), (1, 'L')]

  -- Filter out only those tuples with 'R' (as the 'L's count for nothing)
  let filtered = filter (filterTupleBySecond 'R') zipped

  -- Return the sum of the first (fst) elements in each included tuple
  sum (map fst filtered)

seatId :: String -> Int
seatId str = do
  let row_str = take 7 str -- first 7 chars
  let col_str = drop 7 str -- last 3 chars

  seatRow row_str * 8 + seatCol col_str

main = do
  putStrLn "AoC 2020 - Day 5"

  putStrLn "-- Part 1 Tests"
  test "FBFBBFFRLR" 357 (seatId "FBFBBFFRLR")
  test "BFFFBBFRRR" 567 (seatId "BFFFBBFRRR")
  test "FFFBBBFRRR" 119 (seatId "FFFBBBFRRR")
  test "BBFFBBFRLL" 820 (seatId "BBFFBBFRLL")

  putStrLn "-- Part 1 Solution"
  let seat_ids = map seatId input
  putStrLn $ "Max Seat ID: " ++ show (maximum seat_ids)

  putStrLn "-- Part 2 Solution"
  putStrLn $ "Sorted Seat IDs: " ++ show (sort seat_ids)
  -- For this output, resize terminal window to 10 IDs wide
  -- an quickly skim for where the numbering changes.
  -- 717 is missing.
