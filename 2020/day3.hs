import Data.List

-- Test input:
-- input = ["..##.......", "#...#...#..", ".#....#..#.", "..#.#...#.#", ".#...##..#.", "..#.##.....", ".#.#.#....#", ".#........#", "#.##...#...", "#...##....#", ".#..#...#.#"]

input = [".#......#..####.....#..#......."
       , "#.#...#...#..#.#...#.#...##.##."
       , "#.#....#..........#...##.....##"
       , "#.#.#.....##......#.#.......###"
       , "..#..###....#.#....#.#.#..#...."
       , ".......#.#....##..##...#...#..."
       , "..#..#..#..###.......#.....#.#."
       , ".#.......#...##...##.##......##"
       , "#.#.##..##.#..#....#..###..#.#."
       , "#.....#.#.........#.....##.#.#."
       , "..#.#....##..#...#...##........"
       , "......#....#..##.#.#......###.."
       , ".......#.......#......##...#..."
       , ".##.....#.......#...###.....##."
       , ".#...#.##..##.#..##....#......."
       , "..#......##...#..#...#.#.##.###"
       , ".##.##.....##....#..#......#.#."
       , ".#.....#..###..#.##.#.....##.#."
       , "......##..........#..........#."
       , ".##....#.....#..##.#..#.#..###."
       , "..##.......#....#...##...#..#.."
       , ".##...#.....#.###.#.#..#...#.#."
       , ".....##.#.##..##...#..........."
       , "..#..###.##.#.#.###...###..#.#."
       , ".#........#..#.#........#.#...#"
       , "....##.......#....#.#.##.#....."
       , "....##........######..###..#.#."
       , "#.#.#............#.......#..#.."
       , "...##...#.##.....#.#..#......#."
       , "......#.##.#....##..#.#..###..."
       , "##.....#.#....#....#.##.#.###.."
       , "#..#..#..##.#..##.##.##.#.##..."
       , ".###.####..#..#........#.....##"
       , ".......##..#.......#..........."
       , ".##...#............#.#.##...#.."
       , "....##.....#...##..#..#.#..###."
       , "...#.....#####.#..#...##....##."
       , "#.....#.#.#....##.......##.#.#."
       , "......#.#..#.##.#######......#."
       , "#.##...##....#..###.#.......#.."
       , ".....##...#....#...#....##.##.#"
       , "....###......#...###..#......##"
       , "..#...##..##.######..#.#......#"
       , "......##....#....##..#......##."
       , ".#...#..##..#.###.#......#....#"
       , "##....##..#..####.#.....#...#.."
       , ".#.......#...#.......##......#."
       , "......#...#...#........#......."
       , ".#........#.###...#..####.#..#."
       , "##...#.#............#.....###.."
       , ".....###.#.##...........###..#."
       , ".#.#...#.....#.#.##..##...####."
       , "..##.......#..#.##.#....#.....#"
       , ".#..#.#..####.....###.#.....#.."
       , "..#..###.....####..#.##.#.#.##."
       , ".###..#.....#......#...####...."
       , "...#.#..#.#..#...#...#....##.##"
       , "..###....#.##.....#..........#."
       , "###...#####......##............"
       , "..###.....#........##.#...#..#."
       , "..##.##.#.....##........##..#.#"
       , "##..#.#...#.#..#..###.#....#..#"
       , "....#..#.#.....#..#####...#...."
       , "....#.........#......##.##....."
       , ".#...####.##......##..##.#..#.#"
       , "...#...#.##..#...##..###...#..."
       , "###...#.....#.##.###.###..#.#.."
       , "..#......#.###.....#..##.#...#."
       , "#.....##.########...#####....#."
       , "........##..#..##..##.#........"
       , "....#.######....##..#..#.##..#."
       , "#.......#..##..#..#.#.#..##.##."
       , "...#.#..#..#.......#......###.#"
       , ".#.#..#.#..#.##.#.............#"
       , "#....#.##.#.#.....#..#.#..#...."
       , "...###..#...#....#.........#.#."
       , ".#..#.....##..#.#..#.#.......#."
       , "..#...##...#......#......####.."
       , "....#..#.......#.......#.#..#.."
       , "#...#..#...........#.#..#.....#"
       , "#...#.#.......#...#....###....#"
       , ".#..#.#.##....#......#........#"
       , "..#...#..##..#..#..#..#...#.#.."
       , "..#.#.........#....#....##....."
       , "##.....##.#.#.#.........##....."
       , ".##...#.##...........#...#...##"
       , ".##..##.#.#..........##..##...."
       , "#....#....#.#...#.#..#....#.#.."
       , "####....##.....#..##.###......."
       , "#..#....#......##.#.#....#....."
       , ".....#....#.###.##.........###."
       , "#.......#.####..#..#..##......."
       , "##.#.......#..##..#....#..#.#.."
       , "..###...#.#...#.....##.##.####."
       , "....#...#.#....#..#..#.....#.##"
       , "#.....##.#.#..#.##..#..##......"
       , "................###..#....##..."
       , "..#.##.....#..........##.#...#."
       , "..#.#..#.#....#.#.#..#..#..#.#."
       , "#...#..##.#.#...#..#...#..#...."
       , "#..#.#.........#..###........#."
       , ".#...#.............#..###..#..#"
       , "#.........#.#..#...#.#.....#..#"
       , "....#..#..#.#.#...#...#.....##."
       , "##...###.#.####..#......#...#.."
       , "..#..##...#.#......#.#.......#."
       , "#......###....##.#.##.........."
       , "#####....###..#...............#"
       , "##.#...####....#....#...#....#."
       , ".#.......#..#.....#...#.....###"
       , "...#..#.#.#....##......##...#.."
       , "...#.....#...#.##.#..#.#....#.."
       , "#...###....#...#.#....#........"
       , ".#.......#........#...##.##.##."
       , ".....#....#...##.....##...###.#"
       , "....#....#.#..#...##.##.##....."
       , ".......#............#...#.#..#."
       , ".#............#.....##.......#."
       , "........#....#....##......##.##"
       , ".......##..#.#..#.##..###..##.#"
       , "#..##..##.........####.#.###..."
       , "#....#..#...##...#............."
       , "#...#...###..........##..#..#.."
       , "....#...#..#.....##...#........"
       , "#.....#......#.#.....#...#..#.."
       , "..#.....#.....#....#..#........"
       , "..#..#.....#.#.........#..###.."
       , "................###..#.#....#.."
       , "#.....#.....#.#.#.#.#..#...#.#."
       , "#....#....#.#..........#.#....#"
       , "....#..#......#..##.#...##....."
       , "..#.#...#.####....#.#..#.#..#.."
       , ".........##......#.....##......"
       , "##.#.###.#.....#.....####.#..#."
       , ".....#.....#..#....#..###.#...."
       , "##..#.#...#.##....#....#......."
       , ".....#......#.#...##..#.#......"
       , "....##..#...#...##..##.#....#.#"
       , "............#..........##.#...."
       , "##..#..#.##..##..#.#....#.#.#.."
       , ".......#.#...#...#.#...#..#...."
       , "#....#.#...#...#........#..#..."
       , "...........#.......#...##..###."
       , ".#..##......#.##.........##..#."
       , "...#...#...###.#.##....##.#..#."
       , "#...#..#.#.#.....##..#.......#."
       , ".##..#.###.##......#.#....#.#.#"
       , "..#....#.......#..#..#.#.#.##.."
       , "#...#...###...###.........#...."
       , ".#.#...#.....##.#.#..#....#.##."
       , ".........#.#.##.....#.#.###...."
       , "...#.#...#......#...####......#"
       , "...##..##....##......##...###.."
       , "###...#..#.......##.....#....#."
       , "...#..#..#..###...##.##..#..#.."
       , "...#......#......##..#.#.##..#."
       , "...#.........#....#.#....#.#..."
       , "##................#..#.#.....#."
       , "....#.##...#..#.##...##.#.....#"
       , "......#..##.##..###.#..#.##.##."
       , ".#.#...###.....###.....##...###"
       , ".##.....#.#.#..#..###..#..#..#."
       , "#.......#..#..#....##.....#...."
       , "...#.#.##..#..#......##.##...#."
       , "....##.#......#...#..#..#......"
       , ".####.#..#.....#..##.#...##..##"
       , "..#..#...#..........###..#....#"
       , ".#.#.##.##...#............#...."
       , "........##..##......#.##..#.###"
       , "...#.#....###......##.......#.."
       , "..##...#...#.#..#.....#.....#.."
       , "##..#...###..#..#.#.#...#...#.."
       , ".....#..#....##.....##.....###."
       , "....##...###.#..#.#....##..#..#"
       , "#......#...#....#......#...##.."
       , "....#.##...#.#......#.#.##...#."
       , ".......#.....#...#####...#.#..."
       , "...#.....##.#............#....."
       , "...#.#........#.#.#..#........."
       , "....###......#.#.#..#.####.#..#"
       , "#.....#.#.#.....#.#.#.....#..#."
       , "..##.##......#...#.#..........."
       , "###..###....#.#####......###..."
       , "..##..............##.#.#....#.#"
       , "#..#...#..........#..#.#.#..###"
       , "##.###............#....#.#...#."
       , "#.#..#.#..##.#.#....#...#......"
       , "#....#...#..##.....#..#.#..###."
       , "..#.....#.#....#.#..#.##.#..##."
       , "...##...#.#.##...#....###....#."
       , "......###.####.......#..#.#.#.#"
       , ".#..............##........#...."
       , "...##.##...##....#..#.......#.."
       , ".....#.....#....###...#..#..#.#"
       , ".#.....#..#.....#......#.....##"
       , "#.#.##.#..#..#.....#.##..###..."
       , "..#......#...##.###..#.#...#..#"
       , "......#.....#...##......#......"
       , "##.#........#..........#.....#."
       , "#........##.#............##...."
       , "...#......##...#.#.....##......"
       , "...##.......#....#.#..#.#.###.."
       , "..#....##..##.##.....###....#.."
       , "..#...#.#...#.....#..........#."
       , "......#...#...#.#.##.#...#.#.#."
       , ".#...#......#.##........#......"
       , ".##.##..#....#...#.#...##......"
       , "#..#......#.#...........#....#."
       , "....##.#....#...#..#....#.#..##"
       , "#....##.##....#.#..##.#........"
       , ".##.##.#....##.....#..#....#..#"
       , "...#...#.....###.#.##.........."
       , "....#...#....##.......###......"
       , "#.........#......#.#.......#..."
       , "#..........#..##..#.#.........."
       , ".....#.......#..##.##....##...#"
       , "........................#.#...."
       , "#..#.........#.............#..#"
       , "#..#.....#.......#....#....#.#."
       , "..##..##.......##....#...#....."
       , ".##......#..##......#.###......"
       , "...#.#........#.......##..###.."
       , "..##...###.###......#...#....##"
       , "#...#...#.....###.#.#.#..#....."
       , "#....#.........#..##...#...##.."
       , "#..###..#.#.#.##.#..#.#....#.##"
       , "#...#.#.....#.###.#.......#...."
       , "..##..#..#....#.#...........#.#"
       , "#.........#.#......#...##......"
       , ".######......#..#....#.#.#....#"
       , "##..#.#..####.###.........#...."
       , "###########.....##.##...#..#..."
       , "#...##.#.#....#.#....#......#.."
       , "...#..##..#..##..#......#....#."
       , ".#....#...#....#.#..##....##..."
       , "#..#.#............#....#.#...#."
       , "...#...#..#.#.##......#..#.#..."
       , "#.#...##.....#..#.##......####."
       , ".#.#..##..#.....#.#..#.##......"
       , "#.#.##......##.....#..#.#..#..."
       , "#..##...#.##.#.......#.##......"
       , "..#.......#.#.#...##..##...#..."
       , ".#...#..#..#.#.........#..##..."
       , "#..#.......#....#.#...#.###...#"
       , ".......#..#.......##.#.#...#.#."
       , ".#.................###.#..###.."
       , "..........#.#.....##..#####...#"
       , "#......#.#..##.#.#...#.##.#...."
       , "#......#.#..##.##.#...#....#..."
       , "....#..#......#....#....#######"
       , ".#...#......#....###......#.###"
       , "#.#....#.#...#.###......#..#..#"
       , ".###......#.#...#.####.#..####."
       , "######.#.....###.#...#.#.....#."
       , ".#.###....#..#.#.....#.....####"
       , ".......###.#.........#..#......"
       , "#...#.....##.#......####......."
       , "..#.#..##.#.#...#...#..##..##.."
       , ".....#...##.....#...##......##."
       , "##..#..#.##..#.#......#.....#.."
       , "##.........#.#.##.#..#.#....#.#"
       , ".#........###...#.........#...."
       , "...#..#.#..#....####..........."
       , "#.#....#..##..####.#...#.##...."
       , ".#.....#.......#..........#..##"
       , "...#.......#...###..#.....#..##"
       , ".........#.###.#..##...#.##...#"
       , ".#..........##..####...#..#.#.#"
       , ".#...##...#............##...#.#"
       , "...#....#.#..........#.#..#.#.."
       , ".#.#...##....##.#.#.#....#....."
       , "....#..#.....#.#..#.#..#.##.###"
       , ".....#.#.....#..#......#.#.#..."
       , ".....#.#.#..###..#.#..###...#.."
       , "#.......####...#.#..#......##.#"
       , "....#..#..###......###.##....#."
       , "##.....#.....#.............#..#"
       , "#..#..#...##.....##..#..#.#...."
       , ".....#.#.###...#..............."
       , "#.#.#.....#.#..#.#...#.......#."
       , "..##.##............#....#..##.."
       , "#....##...#.....#.###...#.#...."
       , "#...##.#.........#...#....#...."
       , "##.##.#...#.#...###..#....##..#"
       , "....#....##..#..#.......#...##."
       , ".#...#...#..#.....#..###.#..#.#"
       , "....#..###......#....##....#..."
       , "#.#.....#....##.#..#.#...###..."
       , ".......#............#......#..."
       , ".##..#.###.#.............###..."
       , "..##...##.#.#.#.....#........##"
       , "....#.###....#..#..#...#...#..#"
       , ".....#...#...#..#....#.....##.."
       , "###.#.#.....#......####.....#.."
       , "#.#.###............#......#...."
       , "..#.....#..#..#..#....#......#."
       , "#...######...#....#.##...##.#.#"
       , "##.#.#.#..##......##.#..#.#...#"
       , "............#.#..#.##....#....."
       , "......#............#.#...#..#.#"
       , ".#..##...##..#.#.#..###.....##."
       , "#.###.#...........#...#....#..."
       , "....##.....#...##...#...###.#.#"
       , ".####.#.#.....#.#..#.#.##......"
       , ".#...##......###...#..##..#.#.."
       , ".#......#...#....##.....##..#.."
       , "..........##.....###.##.#...#.#"
       , ".#........##.#..............#.."
       , "#...###..#...#.....#....#.....#"
       , "...#......#..#...#...#..###.#.."
       , ".#...##..#........#.......#.#.."
       , ".#.#.##.........##.##......#..#"
       , "#...#.#.#...#.....#.#...#.#..#."
       , "#.#..#...#...#...##..........#."
       , ".#...........#....#..#.#..#.#.."
       , "#.......#......#..#...#........"
       , ".....#..#...##..###..##........"
       , "......#...#.....#..#.#.#....##."
       , "....##..##..##....###.##......."
       , ".#........##.#.#...#..#........"
       , ".....##...##...#......#..#...#."
       , "..#.....#....###.#..##....#..#."
       , "......#..#...####.#.....##.####"]

cols_count :: Int
cols_count = length (input !! 0)
rows_count :: Int
rows_count = length input

-- encode 'right 3 down 1' logic
col_increment :: Int
col_increment = 3

row_increment :: Int
row_increment = 1

read_tree_increment :: Char -> Int
read_tree_increment terrain = do
  -- `#` implies tree, `.` implies empty space
  if terrain == '#'
  then 1
  else 0

count_trees :: Int -> Int -> Int
count_trees row_num col_num = do
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
    (count_trees (row_num + row_increment) (col_num + col_increment)) + tree_increment

main = do
  putStrLn "AoC 2020 - Day 3"

  putStrLn "-- Part 1 Solution"

  putStrLn $ "Trees encountered: " ++ show (count_trees 1 3)


