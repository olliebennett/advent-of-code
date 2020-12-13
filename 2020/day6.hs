import AoC
import Data.List

-- Blank lines in the input delimit response groups
input = splitStr "\n\n" (read_day_input_raw 6)

test_input = ["abc", "a\nb\nc", "ab\nac", "a\na\na\na", "b"]

alphabet = ['a' .. 'z']

count_unique_responses :: String -> Int
count_unique_responses group_response = do
  -- Count how many of `alphabet` are elements of `group_response`
  length (filter (`elem` group_response) alphabet)

sum_unique_counts :: [String] -> Int
sum_unique_counts group_responses = do
  sum (map count_unique_responses group_responses)

main = do
  putStrLn "AoC 2020 - Day 6"

  putStrLn "-- Part 1 Tests"
  test "Test data counts" 11 (sum_unique_counts test_input)

  putStrLn "-- Part 1 Solution"
  putStrLn $ "Sum of 'yes' answers: " ++ show (sum_unique_counts input)

