import AoC
import Data.List
import Data.Maybe (isNothing)

input = map str2int (splitStr "," (read_day_input_trimmed 15))

next_number :: [Int] -> Int
next_number current_numbers = do
  let last_number = last current_numbers
  let historical_numbers = init current_numbers
  -- Find greatest index of the last number in historical numbers
  let historical_indices = findIndices (== last_number) historical_numbers
  if length historical_indices == 0
  then 0 -- previous number never seen before; set 0
  else do
    -- previous number last seen at `last_index`;
    let last_index = last historical_indices
    let numbers_count = length current_numbers
    numbers_count - last_index - 1

number_2020 :: [Int] -> Int
number_2020 current_numbers = do
  if (length current_numbers) == 2020
  then do
    last current_numbers -- return 2020th element!
  else do
    -- NOTE: Adding an item to the end of a list is ... expensive, and indicates you are building your list in the wrong order. There is usually a better approach.
    -- Source: https://stackoverflow.com/q/42798257/1323144
    number_2020 (current_numbers ++ [(next_number current_numbers)])

main = do
  putStrLn "AoC 2020 - Day 15"

  putStrLn "-- Part 1 Tests"

  test "Turn  4" 0 (next_number [0, 3, 6])
  test "Turn  5" 3 (next_number [0, 3, 6, 0])
  test "Turn  6" 3 (next_number [0, 3, 6, 0, 3])
  test "Turn  7" 1 (next_number [0, 3, 6, 0, 3, 3])
  test "Turn  8" 0 (next_number [0, 3, 6, 0, 3, 3, 1])
  test "Turn  9" 4 (next_number [0, 3, 6, 0, 3, 3, 1, 0])
  test "Turn 10" 0 (next_number [0, 3, 6, 0, 3, 3, 1, 0, 4])

  test "Turn 2020" 436 (number_2020 [0, 3, 6])

  putStrLn "-- Part 1 Solution"

  putStrLn $ "2020th number: " ++ show (number_2020 input)
