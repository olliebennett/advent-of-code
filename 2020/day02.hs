import AoC
import Data.List

input = read_day_input_lines 2

-- Use 'record syntax' to represent each line of input file
-- See: https://haskell.programmingpedia.net/en/tutorial/1950/record-syntax
data ParsedLine = ParsedLine { rangeStart :: Int
                             , rangeEnd :: Int
                             , letter :: Char
                             , password :: [Char] } deriving (Show)

-- Strip specified characters from the string
-- Source: https://rosettacode.org/wiki/Strip_a_set_of_characters_from_a_string#Haskell
-- > stripChars "bd" "abcde"
-- "ace"
stripChars :: String -> String -> String
stripChars = filter . flip notElem

-- Split a string on space characters
-- Adapted from: https://stackoverflow.com/a/46595679/1323144
splitOnSpace str = case break (==' ') str of
                (a, ' ':b) -> a : splitOnSpace b
                (a, "")    -> [a]

-- Split a string on dash (-) characters
splitOnDash str = case break (=='-') str of
                (a, '-':b) -> a : splitOnDash b
                (a, "")    -> [a]

-- Count how many instances of Char `c` there are in String `str`
countChars :: String -> Char -> Int
countChars str char = length (filter (==char) str)

-- Extract data from line
parseLine line = do
  -- Split and parse the line into variables;
  -- Note: We can't use Regex (`Text.Regex`) as not part of standard library!
  let chunks = splitOnSpace line
  let charCountRange = splitOnDash (head chunks)
  let charCountMin = read (head charCountRange) :: Int
  let charCountMax = read (charCountRange !! 1) :: Int
  let charString = stripChars ":" (chunks !! 1)
  let char = head charString
  let passwordString = chunks !! 2

  -- Return a ParsedLine record with the parsed data
  ParsedLine { rangeStart = charCountMin
             , rangeEnd = charCountMax
             , letter = char
             , password = passwordString }

-- Determine (using old rules) whether a line specifies a valid password
-- (i.e. if freq of occurences is within defined range)
validPasswordOld :: String -> Bool
validPasswordOld line = do
  let lineData = parseLine line

  let charCountMin = rangeStart lineData
  let charCountMax = rangeEnd lineData
  let char = letter lineData
  let pwd = password lineData

  -- Count how many instances of `char` there are in `password`
  let numMatches = countChars pwd char

  -- Check the count is within permitted range
  (numMatches >= charCountMin) && (numMatches <= charCountMax)

-- Determine (using new rules) whether a line specifies a valid password
-- (i.e. if xth and yth character match)
validPasswordNew :: String -> Bool
validPasswordNew line = do
  let lineData = parseLine line

  -- Define char positions, compensating for zero-based indices
  let idx1 = rangeStart lineData - 1
  let idx2 = rangeEnd lineData - 1

  let char = letter lineData
  let pwd = password lineData

  let idx1Match = (pwd !! idx1) == char
  let idx2Match = (pwd !! idx2) == char

  -- Check _exactly one_ of the positions contain char
  -- `/=` is XOR operator
  idx1Match /= idx2Match

main = do
  putStrLn "AoC 2020 - Day 2"

  let testA = "1-3 a: abcde"
  let testB = "1-3 b: cdefg"
  let testC = "2-9 c: ccccccccc"

  putStrLn "-- Part 1 Tests"
  test testA True (validPasswordOld testA)
  test testB False (validPasswordOld testB)
  test testC True (validPasswordOld testC)

  putStrLn "-- Part 1 Solution"
  let badPasswordCount = length (filter validPasswordOld input)
  putStrLn $ "Bad password count: " ++ show badPasswordCount

  putStrLn "-- Part 2 Tests"
  test testA True (validPasswordNew testA)
  test testB False (validPasswordNew testB)
  test testC False (validPasswordNew testC)

  putStrLn "-- Part 2 Solution"
  let badPasswordCount = length (filter validPasswordNew input)
  putStrLn $ "Bad password count: " ++ show badPasswordCount
