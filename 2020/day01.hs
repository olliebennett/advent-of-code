-- Similar question: https://stackoverflow.com/a/26163004/1323144
-- This should be possible with folds (reducers); https://wiki.haskell.org/Fold

import AoC
import Data.List

input = map str2int (readDayInputLines 1)

-- Extract all combinations of n elements from a list
-- https://stackoverflow.com/a/8626006/1323144
-- > combinations 2 [1, 2, 3]
-- [[1,2],[1,3],[2,3]]
combinations 0 lst = [[]]
combinations n lst = do
    (x:xs) <- tails lst
    rest   <- combinations (n-1) xs
    return $ x : rest

-- Return True if the input list sums to 2020.
sumTo2020 i = sum i == 2020

main = do
  putStrLn "AoC 2020 - Day 1"

  putStrLn "-- Part 1"
  let pairs = combinations 2 input
  let matches = filter sumTo2020 pairs
  let pair = head matches
  putStrLn $ "Matching pair " ++ show pair ++ " sums to 2020"
  putStrLn $ "Product of matching pair: " ++ show (product pair)

  putStrLn "-- Part 2"
  let triplets = combinations 3 input
  let matches = filter sumTo2020 triplets
  let triplet = head matches
  putStrLn $ "Matching triplet " ++ show triplet ++ " sums to 2020"
  putStrLn $ "Product of matching triplet: " ++ show (product triplet)
