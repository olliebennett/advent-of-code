import AoC
import Data.List

-- Blank lines in the input delimit response groups
input = splitStr "\n\n" (read_day_input_trimmed 6)

test_input = ["abc", "a\nb\nc", "ab\nac", "a\na\na\na", "b"]

alphabet = ['a' .. 'z']

count_any_yes :: String -> Int
count_any_yes group_response_str = do
  -- Count how many of `alphabet` are elements of `group_response_str`
  length (filter (`elem` group_response_str) alphabet)

count_any_yes_sum :: [String] -> Int
count_any_yes_sum group_responses = do
  sum (map count_any_yes group_responses)

in_all_elements :: [String] -> Char -> Bool
in_all_elements group_response_list letter = do
  all (isInfixOf [letter]) group_response_list

count_all_yes :: String -> Int
count_all_yes group_response_str = do
  let group_response_list = splitStr "\n" group_response_str
  -- Get number of characters in `alphabet` which appear in every element of `group_response_list`
  length (filter (in_all_elements group_response_list) alphabet)

count_all_yes_sum :: [String] -> Int
count_all_yes_sum group_responses = do
  let all_yes_counts = map count_all_yes group_responses
  sum all_yes_counts

main = do
  putStrLn "AoC 2020 - Day 6"

  putStrLn "-- Part 1 Test"
  test "Input size" 454 (length input)
  test "Count 'any yes' sum" 11 (count_any_yes_sum test_input)

  putStrLn "-- Part 1 Solution"
  putStrLn $ "Sum of 'any yes' answers: " ++ show (count_any_yes_sum input)

  putStrLn "-- Part 2 Test"
  test "Count 'all yes' in [abc]" 3 (count_all_yes "abc")
  test "Count 'all yes' in [a, b, c]" 0 (count_all_yes "a\nb\nc")
  test "Count 'all yes' in [ab, ac]" 1 (count_all_yes "ab\nac")
  test "Count 'all yes' in [a, a, a, a]" 1 (count_all_yes "a\na\na\na")
  test "Count 'all yes' in [b]" 1 (count_all_yes "b")
  test "Count 'all yes' sum" 6 (count_all_yes_sum test_input)

  putStrLn "-- Part 2 Solution"

  putStrLn $ "Sum of 'all yes' answers: " ++ show (count_all_yes_sum input)
