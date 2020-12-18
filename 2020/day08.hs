import AoC
import Data.List

input = readDayInputLines 8

accumulatorDelta :: (String, Int) -> Int
accumulatorDelta instruction = do
  if fst instruction == "acc" then
    snd instruction
  else
    0 -- only 'acc' has any effect on the accumulator value

indexDelta :: (String, Int) -> Int
indexDelta instruction = do
  if fst instruction == "jmp" then
    -- 'jmp' has custom effect on the index value
    snd instruction
  else
    1 -- other instructions simply increment by 1

processInstructions :: Int -> [(String, Int)] -> [Int] -> Int -> Int
processInstructions idx instructions visited_indexes accumulator = do
  let instruction = instructions !! idx
  let next_idx = idx + indexDelta instruction

  if next_idx `elem` visited_indexes then
    -- `next_idx` has already been visited; return accumulator
    accumulator
  else do
    let updated_visited_indexes = idx : visited_indexes
    let updated_accumulator = accumulator + accumulatorDelta instruction
    -- Recursively iterate
    processInstructions next_idx instructions updated_visited_indexes updated_accumulator

parseInstruction :: String -> (String, Int)
parseInstruction line = do
  let x = splitStr " " line
  let op = head x
  let val = x !! 1
  let num = str2int (tail val)
  if head val == '+' then
    (op, num)
  else
    (op, (-1) * num)

main = do
  putStrLn "AoC 2020 - Day 8"

  putStrLn "-- Part 1 Tests"

  let test_input = ["nop +0"
                  , "acc +1"
                  , "jmp +4"
                  , "acc +3"
                  , "jmp -3"
                  , "acc -99"
                  , "acc +1"
                  , "jmp -4"
                  , "acc +6"]
  let test_instructions = map parseInstruction test_input

  test "parse 'nop +0'" ("nop", 0) (parseInstruction "nop +0")
  test "parse 'acc +4'" ("jmp", 4) (parseInstruction "jmp +4")
  test "parse 'jmp -6'" ("jmp", -6) (parseInstruction "jmp -6")

  test "process" 5 (processInstructions 0 test_instructions [] 0)

  putStrLn "-- Part 1 Solution"
  let instructions = map parseInstruction input
  let visited_indexes = []
  let accumulator = 0
  putStrLn $ "Accumulator val: " ++ show (processInstructions 0 instructions visited_indexes accumulator)

