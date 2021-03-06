module AoC where

import Data.Char (isSpace)
import Data.List (dropWhile, dropWhileEnd, isPrefixOf, lines)
import System.IO.Unsafe (unsafePerformIO)
import Text.Printf (printf)

-- Read day's input file contents to string
-- Note: clearly not 'safe' and not recommended!
-- However, this allows subsequent handling of normal
-- `String`s instead of `IO String`s.
-- See: https://stackoverflow.com/a/17265286/1323144
readDayInputRaw :: Int -> String
readDayInputRaw day_num = do
  let filename = "day" ++ printf "%02d" day_num ++ ".input.txt"
  unsafePerformIO . readFile $ filename

readDayInputTrimmed :: Int -> String
readDayInputTrimmed day_num = do
  trim (readDayInputRaw day_num)

readDayInputLines :: Int -> [String]
readDayInputLines day_num = do
  let raw_input = readDayInputRaw day_num
  -- Split by newlines ...
  let line_list = lines raw_input
  -- ... and remove trailing blank line if any
  if last line_list == ""
  then do
    take (length line_list - 1) line_list
  else do
    line_list

-- Hack(?) to remove 'Maybe' from a result
-- Source: https://www.youtube.com/watch?v=yhKJoFS6EKQ
removeMaybe :: Maybe Int -> [Int]
removeMaybe Nothing = []
removeMaybe (Just i) = [i]

-- String trimming helpers
-- Source: http://rosettacode.org/wiki/Strip_whitespace_from_a_string/Top_and_tail#Haskell
trimLeft :: String -> String
trimLeft = dropWhile isSpace

trimRight :: String -> String
trimRight = dropWhileEnd isSpace

trim :: String -> String
trim = trimLeft . trimRight

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

test :: Eq a => Show a => String -> a -> a -> IO ()
test title expected actual = do
  if expected == actual
    then do
      putStrLn $ "Test: PASS " ++ title ++ " => got " ++ show actual
    else do
      putStrLn $ "Test: FAIL " ++ title ++ " => got " ++ show actual ++ ", expected " ++ show expected
