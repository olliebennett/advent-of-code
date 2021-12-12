import AoC
import Data.List

input = readDayInputRaw 4

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

parseInput :: String -> [String]
parseInput str = do
  -- Blank lines in the input delimit passports
  splitStr "\n\n" str

-- Take "key:value" string and return tuple (key, value)
splitColon :: String -> (String, String)
splitColon str = do
  let parts = splitStr ":" str
  (head parts, parts !! 1)

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
toPassport :: [(String, String)] -> Passport
toPassport kvs = passportTemplate { byr = lookupOrEmptyString "byr" kvs
                                   , iyr = lookupOrEmptyString "iyr" kvs
                                   , eyr = lookupOrEmptyString "eyr" kvs
                                   , hgt = lookupOrEmptyString "hgt" kvs
                                   , hcl = lookupOrEmptyString "hcl" kvs
                                   , ecl = lookupOrEmptyString "ecl" kvs
                                   , pid = lookupOrEmptyString "pid" kvs
                                   , cid = lookupOrEmptyString "cid" kvs }

extractPassport :: String -> [(String, String)]
extractPassport pp_string = do
  -- split string by space or newlines;
  let passport_parts_raw = words pp_string
  -- convert ["a:x", "b:y"] to [(a, x), (b, y)]
  map splitColon passport_parts_raw

buildPassport :: String -> Passport
buildPassport pp_string = toPassport (extractPassport pp_string)

isDigits :: String -> Bool
isDigits = all (`elem` "0123456789")

isHex :: String -> Bool
isHex = all (`elem` "0123456789abcdef")

isWithin :: Int -> Int -> Int -> Bool
isWithin val min max = do
  val >= min && val <= max

-- Birth Year - four digits; at least 1920 and at most 2002.
-- Adapted from: https://stackoverflow.com/a/49464930/1323144
validByr :: String -> Bool
validByr str = do
  length str == 4
    && isDigits str
    && isWithin (str2int str) 1920 2002

-- Issue Year - four digits; at least 2010 and at most 2020.
validIyr :: String -> Bool
validIyr str = do
  length str == 4
    && isDigits str
    && isWithin (str2int str) 2010 2020

-- Expiration Year - four digits; at least 2020 and at most 2030.
validEyr :: String -> Bool
validEyr str = do
  length str == 4
    && isDigits str
    && isWithin (str2int str) 2020 2030

-- Height - a number followed by either cm or in:
-- If cm, the number must be at least 150 and at most 193.
-- If in, the number must be at least 59 and at most 76.
validHgt :: String -> Bool
validHgt str = do
  (length str > 2)
    &&
    (do
      let number_units = splitAt (length str -2) str
      let number_str = fst number_units
      let units = snd number_units
      isDigits number_str
        &&
          (do let number = str2int number_str
              (units == "cm" && isWithin number 150 193)
                || (units == "in" && isWithin number 59 76)))

-- Hair Color - a # followed by exactly six characters 0-9 or a-f.
validHcl :: String -> Bool
validHcl str = do
  length str == 7
    && head str == '#'
    && isHex (tail str)

-- Eye Color - exactly one of: amb blu brn gry grn hzl oth.
validEcl :: String -> Bool
validEcl str = do
  let validEcl_values = ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"]
  str `elem` validEcl_values

-- Passport ID - a nine-digit number, including leading zeroes.
validPid :: String -> Bool
validPid str = do
  length str == 9
    && isDigits str

-- Country ID - ignored, whether missing or not.
validCid :: String -> Bool
validCid cid = do
  True

validPassport :: Passport -> Bool
validPassport passport = do
  -- Passport is valid if NO fields are empty, except cid (which we ignore)
  byr passport /= ""
  && iyr passport /= ""
  && eyr passport /= ""
  && hgt passport /= ""
  && hcl passport /= ""
  && ecl passport /= ""
  && pid passport /= ""

validPassportV2 :: Passport -> Bool
validPassportV2 passport = do
  validByr(byr passport)
  && validIyr(iyr passport)
  && validEyr(eyr passport)
  && validHgt(hgt passport)
  && validHcl(hcl passport)
  && validEcl(ecl passport)
  && validPid(pid passport)
  && validCid(cid passport)

validPassportString :: String -> Bool
validPassportString pp_string = do
  let passport = buildPassport pp_string
  validPassport passport

validPassportString_v2 :: String -> Bool
validPassportString_v2 pp_string = do
  let passport = buildPassport pp_string
  validPassportV2 passport

main = do
  putStrLn "AoC 2020 - Day 4"

  putStrLn "-- Part 1 Test"
  let test_passports = parseInput input_test1
  putStrLn $ "Total valid test passports: " ++ show (length (filter validPassportString test_passports))

  putStrLn "-- Part 1 Solution"
  let passports = parseInput input
  let num_passports = length passports
  let validPassport_count = length (filter validPassportString passports)
  putStrLn $ "Total valid passports: " ++ show validPassport_count ++ " out of " ++ show num_passports

  putStrLn "-- Part 2 Tests"
  test "byr=2002" True (validByr "2002")
  test "byr=2003" False (validByr "2003")
  test "byr=hello" False (validByr "hello")
  test "byr=3" False (validByr "3")
  test "hgt=60in" True (validHgt "60in")
  test "hgt=190cm" True (validHgt "190cm")
  test "hgt=190in" False (validHgt "190in")
  test "hgt=190" False (validHgt "190")
  test "hcl=#123abc" True (validHcl "#123abc")
  test "hcl=#123abz" False (validHcl "#123abz")
  test "hcl=123abc" False (validHcl "123abc")
  test "ecl=brn" True (validEcl "brn")
  test "ecl=wat" False (validEcl "wat")
  test "pid=000000001" True (validPid "000000001")
  test "pid=0123456789" False (validPid "0123456789")

  putStrLn "-- Part 2 Solution"
  let validPassport_count_v2 = length (filter validPassportString_v2 passports)
  putStrLn $ "Total valid passports: " ++ show validPassport_count_v2 ++ " out of " ++ show num_passports
