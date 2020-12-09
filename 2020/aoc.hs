module AoC where

import Data.List
import System.IO.Unsafe

-- Read day's input file contents to string
-- Note: clearly not 'safe' and not recommended!
-- However, this allows subsequent handling of normal
-- `String`s instead of `IO String`s.
-- See: https://stackoverflow.com/a/17265286/1323144
read_day_input_raw :: Int -> String
read_day_input_raw day_num = do
  let filename = "day" ++ show day_num ++ ".input.txt"
  unsafePerformIO . readFile $ filename

read_day_input_lines :: Int -> [String]
read_day_input_lines day_num = do
  let raw_input = read_day_input_raw day_num
  -- Split by newlines ...
  let lines = splitStr "\n" raw_input
  -- ... and remove trailing blank line if any
  if last lines == ""
  then do
    take (length lines - 1) lines
  else do
    lines

-- Split a string on given sub-string
-- Source: https://gist.github.com/kevinadi/da8fbd30b3e03300ce56
splitStr :: Eq a => [a] -> [a] -> [[a]]
splitStr sub str = split' sub str [] []
    where
    split' _   []  subacc acc = reverse (reverse subacc:acc)
    split' sub str subacc acc
        | sub `isPrefixOf` str = split' sub (drop (length sub) str) [] (reverse subacc:acc)
        | otherwise            = split' sub (tail str) (head str:subacc) acc

str2int :: String -> Int
str2int str = do
  read str :: Int
