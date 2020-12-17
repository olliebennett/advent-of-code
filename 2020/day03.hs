import AoC
import Data.List

-- Test input:
-- input = ["..##.......", "#...#...#..", ".#....#..#.", "..#.#...#.#", ".#...##..#.", "..#.##.....", ".#.#.#....#", ".#........#", "#.##...#...", "#...##....#", ".#..#...#.#"]

input = read_day_input_lines 3

cols_count :: Int
cols_count = length (input !! 0)
rows_count :: Int
rows_count = length input

read_tree_increment :: Char -> Int
read_tree_increment terrain = do
  -- `#` implies tree, `.` implies empty space
  if terrain == '#'
  then 1
  else 0

count_trees :: Int -> Int -> Int -> Int -> Int
count_trees row_num row_incr col_num col_incr = do
  -- Stop ongoing recursion once we processed all rows
  if row_num >= rows_count
    then 0 -- no extra trees
  else do
    let row_data = input !! row_num

    -- Since the cols data repeats horizontally, we take modulo
    let col_idx = mod col_num cols_count

    let terrain = row_data !! col_idx
    let tree_increment = read_tree_increment terrain

    -- Recursively process next iteration, eventually returning total tree count
    (count_trees (row_num + row_incr) row_incr (col_num + col_incr) col_incr) + tree_increment

main = do
  putStrLn "AoC 2020 - Day 3"

  putStrLn "-- Part 1 Solution"

  putStrLn $ "Trees encountered: " ++ show (count_trees 1 1 3 3)

  putStrLn "-- Part 2 Solution"
  -- [(1, 1), (1, 3), (1, 5), (1, 7), (2, 1)]
  let trees_1_1 = count_trees 1 1 1 1
  let trees_1_3 = count_trees 1 1 3 3
  let trees_1_5 = count_trees 1 1 5 5
  let trees_1_7 = count_trees 1 1 7 7
  let trees_2_1 = count_trees 2 2 1 1
  putStrLn $ "Trees encountered at 1/1: " ++ show trees_1_1
  putStrLn $ "Trees encountered at 1/3: " ++ show trees_1_3
  putStrLn $ "Trees encountered at 1/5: " ++ show trees_1_5
  putStrLn $ "Trees encountered at 1/7: " ++ show trees_1_7
  putStrLn $ "Trees encountered at 2/1: " ++ show trees_2_1

  let prod = product [trees_1_1, trees_1_3, trees_1_5, trees_1_7, trees_2_1]
  putStrLn $ "Product of trees: " ++ show prod
