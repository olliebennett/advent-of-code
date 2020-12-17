import AoC
import Data.List
import Data.Map (Map)
import qualified Data.Map as Map (empty, fromList, insert, lookup)
import Data.Maybe (isNothing)

input = map str2int (splitStr "," (readDayInputTrimmed 15))

nextVal :: Int -> Int -> Map Int Int -> Int
nextVal current_turn latest_val history = do
  -- Extract the last time this value was seen
  let previous_occurrence_maybe = Map.lookup latest_val history
  if isNothing previous_occurrence_maybe
  then 0 -- number never previously seen; set 0
  else do
    -- Hack to remove `Maybe` as we've already checked with isNothing
    let previous_occurrence = head (removeMaybe previous_occurrence_maybe)
    current_turn - previous_occurrence - 1

nthNumber :: Int -> Int -> Int -> Map Int Int -> Int
nthNumber target_turn num_completed_turns latest_val history = do
  let current_val = nextVal (num_completed_turns + 1) latest_val history
  let next_turn = num_completed_turns + 1
  if next_turn == target_turn
  then do
    -- we've reached the required N; return the value!
    current_val
  else do
    -- Insert (or replace) the current turn's data into the map of historical occurrences
    let updated_history = Map.insert latest_val num_completed_turns history
    -- And iterate recursively for the next turn
    nthNumber target_turn next_turn current_val updated_history

main = do
  putStrLn "AoC 2020 - Day 15"

  putStrLn "-- Part 1 Tests"

  -- Manually build history from test results
  let test_history0 = Map.empty
  let test_history1 = Map.insert 0 1 test_history0
  let test_history2 = Map.insert 3 2 test_history1
  let test_history3 = Map.insert 6 3 test_history2
  let test_history4 = Map.insert 0 4 test_history3
  let test_history5 = Map.insert 3 5 test_history4
  let test_history6 = Map.insert 3 6 test_history5
  let test_history7 = Map.insert 1 7 test_history6
  let test_history8 = Map.insert 0 8 test_history7
  let test_history9 = Map.insert 4 9 test_history8

  putStrLn "Tests: Next only (single iteration)"
  test "Turn (next)  4" 0 (nextVal  4 6 test_history2)
  test "Turn (next)  5" 3 (nextVal  5 0 test_history3)
  test "Turn (next)  6" 3 (nextVal  6 3 test_history4)
  test "Turn (next)  7" 1 (nextVal  7 3 test_history5)
  test "Turn (next)  8" 0 (nextVal  8 1 test_history6)
  test "Turn (next)  9" 4 (nextVal  9 0 test_history7)
  test "Turn (next) 10" 0 (nextVal 10 4 test_history8)
  putStrLn "Tests: Full (recursive iteration)"
  test "Turn (full)  4" 0 (nthNumber  4 3 6 test_history2)
  test "Turn (full)  5" 3 (nthNumber  5 3 6 test_history2)
  test "Turn (full)  6" 3 (nthNumber  6 3 6 test_history2)
  test "Turn (full)  7" 1 (nthNumber  7 3 6 test_history2)
  test "Turn (full)  8" 0 (nthNumber  8 3 6 test_history2)
  test "Turn (full)  9" 4 (nthNumber  9 3 6 test_history2)
  test "Turn (full) 10" 0 (nthNumber 10 3 6 test_history2)

  putStrLn "-- Part 1 Solution"

  -- Manually build history for input data
  let history0 = Map.empty
  let history1 = Map.insert 0 1 history0
  let history2 = Map.insert 3 2 history1
  let history3 = Map.insert 1 3 history2
  let history4 = Map.insert 6 4 history3
  let history5 = Map.insert 7 5 history4

  putStrLn $ "Turn 2020: " ++ show (nthNumber 2020 6 5 history5)

  putStrLn "-- Part 2 Test"
  putStrLn $ "Turn 1M (warm up): " ++ show (nthNumber 100000 6 5 history5)

  putStrLn "-- Part 2 Solution"
  putStrLn $ "Turn 30M: " ++ show (nthNumber 30000000 6 5 history5)
