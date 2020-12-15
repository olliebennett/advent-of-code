import AoC
import Data.List
import Data.Maybe (isNothing)

input = map str2int (splitStr "," (read_day_input_trimmed 15))

next_number :: [Int] -> Int
next_number current_numbers = do
  let prev_number = head current_numbers -- first element
  let historical_numbers = tail current_numbers -- remaining elements
  -- Find most recent (first) occurrence of prev number in historical list
  let last_index = findIndex (== prev_number) historical_numbers
  if isNothing last_index
  then 0 -- number never previously seen; set 0
  else do
    -- `last_index` is the index of last occurrence of the previous number;
    -- Add 1 to negate 0-based indexing
    (head (removeMaybe last_index)) + 1

nth_number :: [Int] -> Int -> Int
nth_number current_numbers n = do
  if (length current_numbers) == n
  then do
    head current_numbers -- we have the nth element (first in list)!
  else do
    let next_n = next_number current_numbers
    let new_list = next_n : current_numbers
    nth_number new_list n

main = do
  putStrLn "AoC 2020 - Day 15"

  putStrLn "-- Part 1 Tests"

  test "Turn  4" 0 (next_number (reverse [0, 3, 6]))
  test "Turn  5" 3 (next_number (reverse [0, 3, 6, 0]))
  test "Turn  6" 3 (next_number (reverse [0, 3, 6, 0, 3]))
  test "Turn  7" 1 (next_number (reverse [0, 3, 6, 0, 3, 3]))
  test "Turn  8" 0 (next_number (reverse [0, 3, 6, 0, 3, 3, 1]))
  test "Turn  9" 4 (next_number (reverse [0, 3, 6, 0, 3, 3, 1, 0]))
  test "Turn 10" 0 (next_number (reverse [0, 3, 6, 0, 3, 3, 1, 0, 4]))

  test "Turn 2020" 436 (nth_number (reverse [0, 3, 6]) 2020)

  putStrLn "-- Part 1 Solution"

  putStrLn $ "2020th number: " ++ show (nth_number (reverse input) 2020)

  putStrLn "-- Part 2 Tests"

  test "Turn 30,000,000" 175594 (nth_number (reverse [0, 3, 6]) 50000)
