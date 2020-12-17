import AoC
import Data.List

-- Blank lines in the input delimit response groups
input = splitStr "\n\n" (read_day_input_trimmed 6)

test_input = ["abc", "a\nb\nc", "ab\nac", "a\na\na\na", "b"]

alphabet = ['a' .. 'z']

countAnyYes :: String -> Int
countAnyYes group_response_str = do
  -- Count how many of `alphabet` are elements of `group_response_str`
  length (filter (`elem` group_response_str) alphabet)

countAnyYesSum :: [String] -> Int
countAnyYesSum group_responses = do
  sum (map countAnyYes group_responses)

inAllElements :: [String] -> Char -> Bool
inAllElements group_response_list letter = do
  all (isInfixOf [letter]) group_response_list

countAllYes :: String -> Int
countAllYes group_response_str = do
  let group_response_list = splitStr "\n" group_response_str
  -- Get number of characters in `alphabet` which appear in every element of `group_response_list`
  length (filter (inAllElements group_response_list) alphabet)

countAllYesSum :: [String] -> Int
countAllYesSum group_responses = do
  let all_yes_counts = map countAllYes group_responses
  sum all_yes_counts

main = do
  putStrLn "AoC 2020 - Day 6"

  putStrLn "-- Part 1 Test"
  test "Input size" 454 (length input)
  test "Count 'any yes' sum" 11 (countAnyYesSum test_input)

  putStrLn "-- Part 1 Solution"
  putStrLn $ "Sum of 'any yes' answers: " ++ show (countAnyYesSum input)

  putStrLn "-- Part 2 Test"
  test "Count 'all yes' in [abc]" 3 (countAllYes "abc")
  test "Count 'all yes' in [a, b, c]" 0 (countAllYes "a\nb\nc")
  test "Count 'all yes' in [ab, ac]" 1 (countAllYes "ab\nac")
  test "Count 'all yes' in [a, a, a, a]" 1 (countAllYes "a\na\na\na")
  test "Count 'all yes' in [b]" 1 (countAllYes "b")
  test "Count 'all yes' sum" 6 (countAllYesSum test_input)

  putStrLn "-- Part 2 Solution"

  putStrLn $ "Sum of 'all yes' answers: " ++ show (countAllYesSum input)
