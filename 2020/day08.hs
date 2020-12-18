import AoC
import Data.List

input = readDayInputLines 8

data Instruction = Instruction { op :: String, idx :: Int } deriving (Eq, Show)

isAcc :: Instruction -> Bool
isAcc instruction = do
  op instruction == "acc"

isJmp :: Instruction -> Bool
isJmp instruction = do
  op instruction == "jmp"

isNop :: Instruction -> Bool
isNop instruction = do
  op instruction == "nop"

accumulatorDelta :: Instruction -> Int
accumulatorDelta instruction = do
  if isAcc instruction then
    idx instruction
  else
    0 -- only 'acc' has any effect on the accumulator value

indexDelta :: Instruction -> Int
indexDelta instruction = do
  if isJmp instruction then
    idx instruction
  else
    1 -- other instructions simply increment by 1

processInstructions :: Int -> [Instruction] -> [Int] -> Int -> Int
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

parseInstruction :: String -> Instruction
parseInstruction line = do
  let x = splitStr " " line
  let val = x !! 1
  let num = str2int (tail val)
  if head val == '+' then
    Instruction { op = head x, idx = num }
  else
    Instruction { op = head x, idx = (-1) * num }

main = do
  putStrLn "AoC 2020 - Day 8"

  putStrLn "-- Part 1 Tests"

  let test_input = ["nop +0", "acc +1", "jmp +4", "acc +3", "jmp -3", "acc -99", "acc +1", "jmp -4", "acc +6"]
  let test_instructions = map parseInstruction test_input

  test "parse 'nop +0'" Instruction { op = "nop", idx = 0 } (parseInstruction "nop +0")
  test "parse 'acc +4'" Instruction { op = "jmp", idx = 4 } (parseInstruction "jmp +4")
  test "parse 'jmp -6'" Instruction { op = "jmp", idx = -6 } (parseInstruction "jmp -6")

  test "process" 5 (processInstructions 0 test_instructions [] 0)

  putStrLn "-- Part 1 Solution"
  let instructions = map parseInstruction input
  let visited_indexes = []
  let accumulator = 0
  let result = processInstructions 0 instructions visited_indexes accumulator
  putStrLn $ "Accumulator before crash val: " ++ show result

  putStrLn "-- Part 2 Test"
