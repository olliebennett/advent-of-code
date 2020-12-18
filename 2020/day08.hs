import AoC
import Data.List

input = readDayInputLines 8

data Instruction = Instruction { op :: String, delta :: Int } deriving (Eq, Show)

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
    delta instruction
  else
    0 -- only 'acc' has any effect on the accumulator value

indexDelta :: Instruction -> Int
indexDelta instruction = do
  if isJmp instruction then
    delta instruction
  else
    1 -- other instructions simply increment by 1

-- Extracts the `idx`th Instruction, flipping if required
extractInstruction :: [Instruction] -> Int -> Int -> Instruction
extractInstruction instructions index_to_flip idx = do
  if idx == index_to_flip then
    flipInstruction (instructions !! idx)
  else do
    instructions !! idx

-- Returns a tuple with (a, b), where
-- a = `True` if termination was successful (or `False` if instructions loop infinitely)
-- b = Accumulator value at termination
processInstructions :: Int -> Int -> [Instruction] -> [Int] -> Int -> (Bool, Int)
processInstructions index_to_flip idx instructions visited_indexes accumulator = do
  let instruction = extractInstruction instructions index_to_flip idx
  let updated_accumulator = accumulator + accumulatorDelta instruction
  let next_idx = idx + indexDelta instruction
  if next_idx == length instructions then
    (True, updated_accumulator) -- break with success=True
  else do
    if next_idx `elem` visited_indexes then
      -- `next_idx` has already been visited; break with success=False
      (False, updated_accumulator)
    else do
      let updated_visited_indexes = idx : visited_indexes
      -- Recursively iterate
      processInstructions index_to_flip next_idx instructions updated_visited_indexes updated_accumulator

parseInstruction :: String -> Instruction
parseInstruction line = do
  let x = splitStr " " line
  let val = x !! 1
  let num = str2int (tail val) -- tail omits first char
  if head val == '+' then
    Instruction { op = head x, delta = num }
  else
    Instruction { op = head x, delta = (-1) * num }

-- Build list of indexes for instructions that have not yet been flip-tested
buildUntestedIndexes :: [Instruction] -> [Int]
buildUntestedIndexes instructions = do
  let flippable = map isAcc instructions
  -- Return the corresponding indexes of the `False` (non-acc) values
  elemIndices False flippable

flipInstruction :: Instruction -> Instruction
flipInstruction instruction = do
  let num = delta instruction
  if isJmp instruction then do
    Instruction { op = "nop", delta = num }
  else do
    -- assume it was "nop"; flip to "jmp"
    Instruction { op = "jmp", delta = num }

-- Flip each flippable instruction in turn, checking result of processing the modified instruction
flipAndCheck :: [Instruction] -> [Int] -> Int
flipAndCheck instructions untested_flips = do
  -- Find and flip the next non-flipped instruction
  let next_untested_idx = head untested_flips

  -- Trigger a new recursive call chain to `processInstructions`, with initial conditions
  let result_tuple = processInstructions next_untested_idx 0 instructions [] 0
  let successful_termination = fst result_tuple
  -- `True` indicates successful (non-crashing) termination of program
  if successful_termination then do
    snd result_tuple -- final accumulator value
  else do
    -- No luck; modify the next flippable instruction and recurse
    let remaining_untested_idxs = tail untested_flips
    flipAndCheck instructions remaining_untested_idxs

main = do
  putStrLn "AoC 2020 - Day 8"

  putStrLn "-- Part 1 Tests"

  let test_input = ["nop +0", "acc +1", "jmp +4", "acc +3", "jmp -3", "acc -99", "acc +1", "jmp -4", "acc +6"]
  let test_instructions = map parseInstruction test_input

  test "parse 'nop +0'" Instruction { op = "nop", delta = 0 } (parseInstruction "nop +0")
  test "parse 'acc +4'" Instruction { op = "jmp", delta = 4 } (parseInstruction "jmp +4")
  test "parse 'jmp -6'" Instruction { op = "jmp", delta = -6 } (parseInstruction "jmp -6")

  -- (-1) prevents flipping any index
  let test1a_result = processInstructions (-1) 0 test_instructions [] 0
  test "process" 5 (snd test1a_result)

  putStrLn "-- Part 1 Solution"
  let instructions = map parseInstruction input
  let visited_indexes = []
  let accumulator = 0
  let result1 = processInstructions (-1) 0 instructions visited_indexes accumulator
  putStrLn $ "Accumulator before crash val: " ++ show (snd result1)

  putStrLn "-- Part 2 Tests"

  test "extractInstruction flip nop" Instruction { op = "jmp", delta = 0 } (extractInstruction test_instructions 0 0)
  test "extractInstruction flip jmp" Instruction { op = "nop", delta = 4 } (extractInstruction test_instructions 2 2)
  test "extractInstruction no flip" Instruction { op = "nop", delta = 0 } (extractInstruction test_instructions (-1) 0)

  let test_untested_flips = buildUntestedIndexes test_instructions
  test "final accumulator" 8 (flipAndCheck test_instructions test_untested_flips)

  putStrLn "-- Part 2 Solution"

  let untested_flips = buildUntestedIndexes instructions
  let result2 = flipAndCheck instructions untested_flips
  putStrLn $ "Accumulator after corrected processing: " ++ show result2
