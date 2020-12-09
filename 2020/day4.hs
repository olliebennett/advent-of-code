import AoC
import Data.List

input = read_day_input_raw 4

input_test1 = "ecl:gry pid:860033327 eyr:2020 hcl:#fffffd\nbyr:1937 iyr:2017 cid:147 hgt:183cm\n\niyr:2013 ecl:amb cid:350 eyr:2023 pid:028048884\nhcl:#cfa07d byr:1929\n\nhcl:#ae17e1 iyr:2013\neyr:2024\necl:brn pid:760753108 byr:1931\nhgt:179cm\n\nhcl:#cfa07d eyr:2025 pid:166559648\niyr:2011 ecl:brn hgt:59in"

data Passport = Passport { byr :: String -- Birth Year
                         , iyr :: String -- Issue Year
                         , eyr :: String -- Expiration Year
                         , hgt :: String -- Height
                         , hcl :: String -- Hair Color
                         , ecl :: String -- Eye Color
                         , pid :: String -- Passport ID
                         , cid :: String } deriving (Show) -- Country ID

-- Define non-Nothing 'default values' in advance
-- to avoid 'Fields of ‘Passport’ not initialised' warnings
-- Source: https://wiki.haskell.org/Default_values_in_records
passportTemplate = Passport { byr = "", iyr = "", eyr = "", hgt = "", hcl = "", ecl = "", pid = "", cid = "" }

parse_input :: String -> [String]
parse_input str = do
  -- Blank lines in the input delimit passports
  splitStr "\n\n" str

-- Take "key:value" string and return tuple (key, value)
splitColon :: String -> (String, String)
splitColon str = do
  let parts = splitStr ":" str
  ((parts !! 0), (parts !! 1))

-- Lookup a key in association list, returning empty string if not found
-- Adapted from 'lookup' in Haskell base package;
-- http://hackage.haskell.org/package/base-4.8.1.0/docs/src/GHC.List.html#lookup
lookupOrEmptyString :: (Eq a) => a -> [(a,String)] -> String
lookupOrEmptyString _key [] = ""
lookupOrEmptyString key ((x,y):xys)
    | key == x = y
    | otherwise = lookupOrEmptyString key xys

-- Take key-value tuples and write into a Passport record
-- Adapted from https://stackoverflow.com/a/37859036/1323144
-- Note: We use an empty string fallback because the `Maybe` approach (using
-- vanilla `lookup` which returns `Maybe a`, incl perhaps `Nothing`) causes
-- a "Missing field in record construction" error
to_passport :: [(String, String)] -> Passport
to_passport kvs = passportTemplate { byr = (lookupOrEmptyString "byr" kvs)
                                   , iyr = (lookupOrEmptyString "iyr" kvs)
                                   , eyr = (lookupOrEmptyString "eyr" kvs)
                                   , hgt = (lookupOrEmptyString "hgt" kvs)
                                   , hcl = (lookupOrEmptyString "hcl" kvs)
                                   , ecl = (lookupOrEmptyString "ecl" kvs)
                                   , pid = (lookupOrEmptyString "pid" kvs)
                                   , cid = (lookupOrEmptyString "cid" kvs) }

extract_passport :: String -> [(String, String)]
extract_passport pp_string = do
  -- split string by space or newlines;
  let passport_parts_raw = words pp_string
  -- convert ["a:x", "b:y"] to [(a, x), (b, y)]
  map splitColon passport_parts_raw

build_passport :: String -> Passport
build_passport pp_string = do
  to_passport (extract_passport pp_string)

-- Birth Year - four digits; at least 1920 and at most 2002.
-- Adapted from: https://stackoverflow.com/a/49464930/1323144
valid_byr :: String -> Bool
valid_byr str = do
  length str == 4
    && (all (`elem` "0123456789") str)
    && (read str :: Int) >= 1920
    && (read str :: Int) <= 2002

-- Issue Year - four digits; at least 2010 and at most 2020.
valid_iyr :: String -> Bool
valid_iyr str = do
  length str == 4
    && (all (`elem` "0123456789") str)
    && (read str :: Int) >= 2010
    && (read str :: Int) <= 2020

-- Expiration Year - four digits; at least 2020 and at most 2030.
valid_eyr :: String -> Bool
valid_eyr str = do
  length str == 4
    && (all (`elem` "0123456789") str)
    && (read str :: Int) >= 2020
    && (read str :: Int) <= 2030

-- Height - a number followed by either cm or in:
-- If cm, the number must be at least 150 and at most 193.
-- If in, the number must be at least 59 and at most 76.
valid_hgt :: String -> Bool
valid_hgt str = do
  if length str <= 2
    then False
    else do
      let units = drop (length str -2) str
      let number_str = take (length str -2) str
      if (all (`elem` "0123456789") number_str) && (elem units ["in", "cm"])
        then do
          let number = read number_str :: Int
          if units == "cm"
            then number >= 150 && number <= 193
            else number >= 59 && number <= 76
        else False -- invalid number format or unit

-- Hair Color - a # followed by exactly six characters 0-9 or a-f.
valid_hcl :: String -> Bool
valid_hcl str = do
  length str == 7
    && str !! 0 == '#'
    && (all (`elem` "0123456789abcdef") (drop 1 str))

-- Eye Color - exactly one of: amb blu brn gry grn hzl oth.
valid_ecl :: String -> Bool
valid_ecl str = do
  let valid_ecl_values = ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"]
  elem str valid_ecl_values

-- Passport ID - a nine-digit number, including leading zeroes.
valid_pid :: String -> Bool
valid_pid str = do
  length str == 9
    && (all (`elem` "0123456789") str)

-- Country ID - ignored, whether missing or not.
valid_cid :: String -> Bool
valid_cid cid = do
  True

valid_passport :: Passport -> Bool
valid_passport passport = do
  -- Passport is valid if NO fields are empty, except cid (which we ignore)
  byr passport /= ""
  && iyr passport /= ""
  && eyr passport /= ""
  && hgt passport /= ""
  && hcl passport /= ""
  && ecl passport /= ""
  && pid passport /= ""

valid_passport_v2 :: Passport -> Bool
valid_passport_v2 passport = do
  valid_byr(byr passport)
  && valid_iyr(iyr passport)
  && valid_eyr(eyr passport)
  && valid_hgt(hgt passport)
  && valid_hcl(hcl passport)
  && valid_ecl(ecl passport)
  && valid_pid(pid passport)
  && valid_cid(cid passport)

valid_passport_string :: String -> Bool
valid_passport_string pp_string = do
  let passport = build_passport pp_string
  valid_passport passport

valid_passport_string_v2 :: String -> Bool
valid_passport_string_v2 pp_string = do
  let passport = build_passport pp_string
  valid_passport_v2 passport

main = do
  putStrLn "AoC 2020 - Day 4"

  putStrLn "-- Part 1 Test"
  let test_passports = parse_input input_test1
  putStrLn $ "Total valid test passports: " ++ show (length (filter valid_passport_string test_passports))

  putStrLn "-- Part 1 Solution"
  let passports = parse_input input
  let num_passports = length passports
  let valid_passport_count = length (filter valid_passport_string passports)
  putStrLn $ "Total valid passports: " ++ show valid_passport_count ++ " out of " ++ show num_passports

  putStrLn "-- Part 2 Tests"
  test "byr=2002" True (valid_byr "2002")
  test "byr=2003" False (valid_byr "2003")
  test "byr=hello" False (valid_byr "hello")
  test "byr=3" False (valid_byr "3")
  test "hgt=60in" True (valid_hgt "60in")
  test "hgt=190cm" True (valid_hgt "190cm")
  test "hgt=190in" False (valid_hgt "190in")
  test "hgt=190" False (valid_hgt "190")
  test "hcl=#123abc" True (valid_hcl "#123abc")
  test "hcl=#123abz" False (valid_hcl "#123abz")
  test "hcl=123abc" False (valid_hcl "123abc")
  test "ecl=brn" True (valid_ecl "brn")
  test "ecl=wat" False (valid_ecl "wat")
  test "pid=000000001" True (valid_pid "000000001")
  test "pid=0123456789" False (valid_pid "0123456789")

  putStrLn "-- Part 2 Solution"
  let valid_passport_count_v2 = length (filter valid_passport_string_v2 passports)
  putStrLn $ "Total valid passports: " ++ show valid_passport_count_v2 ++ " out of " ++ show num_passports
