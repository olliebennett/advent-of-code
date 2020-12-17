import AoC
import Data.List

input = read_day_input_lines 7

test_input = ["light red bags contain 1 bright white bag, 2 muted yellow bags."
            , "dark orange bags contain 3 bright white bags, 4 muted yellow bags."
            , "bright white bags contain 1 shiny gold bag."
            , "muted yellow bags contain 2 shiny gold bags, 9 faded blue bags."
            , "shiny gold bags contain 1 dark olive bag, 2 vibrant plum bags."
            , "dark olive bags contain 3 faded blue bags, 4 dotted black bags."
            , "vibrant plum bags contain 5 faded blue bags, 6 dotted black bags."
            , "faded blue bags contain no other bags."
            , "dotted black bags contain no other bags."]

data BagCount = BagCount { bag_colour :: String, count :: Int } deriving (Eq, Show)

data BagRule = BagRule { rule_colour :: String, sub_bags :: [BagCount] } deriving (Eq, Show)

shiny_gold = "shiny gold"

parse_bag_count :: String -> BagCount
parse_bag_count bag_count_str = do
  let useful = (splitStr " bag" bag_count_str) !! 0
  let str_words = words useful
  let bag_cnt = str2int (head str_words)
  let bag_clr = unwords (tail str_words)
  BagCount { count = bag_cnt, bag_colour = bag_clr }

parse_bag_counts :: String -> [BagCount]
parse_bag_counts "no other bags." = [] -- no BagCounts
parse_bag_counts bag_counts_str = do
  let bag_count_strings = splitStr ", " bag_counts_str
  map parse_bag_count bag_count_strings

parse_bag_rule :: String -> BagRule
parse_bag_rule rule = do
  let rule_split = splitStr " bags contain " rule
  let colour = rule_split !! 0
  let bag_counts = parse_bag_counts (rule_split !! 1)
  BagRule { rule_colour = colour, sub_bags = bag_counts }

parse_bag_rules :: [String] -> [BagRule]
parse_bag_rules rules_list = do
  map parse_bag_rule rules_list

colour_match :: String -> BagRule -> Bool
colour_match colour bag_rule = do
  rule_colour bag_rule == colour

colour_2_bag_rule :: [BagRule] -> String -> BagRule
colour_2_bag_rule bag_rules clr = do
  head (filter (colour_match clr) bag_rules)

is_shiny_gold :: BagCount -> Bool
is_shiny_gold bag_count = do
  (bag_colour bag_count) == shiny_gold

contains_shiny_gold :: [BagRule] -> BagRule -> Bool
contains_shiny_gold all_bag_rules bag_rule = do
  let sub_bag_counts = sub_bags bag_rule
  if any is_shiny_gold sub_bag_counts
  then True
  else do
    if length sub_bag_counts == 0
    then False
    else do
      let sub_bag_colours = map bag_colour sub_bag_counts
      let sub_bag_rules = map (colour_2_bag_rule all_bag_rules) sub_bag_colours
      -- Recursively check 'child' bag rules for
      any (contains_shiny_gold all_bag_rules) sub_bag_rules

count_shiny_golds :: [BagRule] -> Int
count_shiny_golds bag_rules = do
  length (filter (contains_shiny_gold bag_rules) bag_rules)

nesting_count_single :: [BagRule] -> BagCount -> Int
nesting_count_single all_bag_rules bag_count = do
  let cnt = count bag_count
  let bag_col = bag_colour bag_count
  let bag_rule = colour_2_bag_rule all_bag_rules bag_col
  let bags = sub_bags bag_rule
  -- Count child bags, plus (count * number of each of their children)
  cnt + cnt * (nesting_count all_bag_rules bags)

nesting_count :: [BagRule] -> [BagCount] -> Int
nesting_count all_bag_rules bag_counts = do
  sum (map (nesting_count_single all_bag_rules) bag_counts)

main = do
  putStrLn "AoC 2020 - Day 7"

  putStrLn "-- Part 1 Tests"

  let test_bag_count1 = BagCount { count = 5, bag_colour = "faded blue" }
  let test_bag_count2 = BagCount { count = 1, bag_colour = "shiny maroon" }
  test "parse bag count A" test_bag_count1 (parse_bag_count "5 faded blue bags,")
  test "parse bag count B" test_bag_count2 (parse_bag_count "1 shiny maroon bag.")

  let test_bag_counts = [test_bag_count1, test_bag_count2]
  test "parse bag counts" test_bag_counts (parse_bag_counts "5 faded blue bags, 1 shiny maroon bag.")

  let test_bag_rule_str = "clear lavender bags contain 5 faded blue bags, 1 shiny maroon bag."
  let test_bag_rule = BagRule { rule_colour = "clear lavender", sub_bags = test_bag_counts }
  test "parse bag rule" test_bag_rule (parse_bag_rule test_bag_rule_str)

  test "parse bag rules" [test_bag_rule] (parse_bag_rules [test_bag_rule_str])

  let test_bag_rules = parse_bag_rules test_input

  test "light red" True (contains_shiny_gold test_bag_rules (test_bag_rules !! 0))
  test "dark orange" True (contains_shiny_gold test_bag_rules (test_bag_rules !! 1))
  test "bright white" True (contains_shiny_gold test_bag_rules (test_bag_rules !! 2))
  test "muted yellow" True (contains_shiny_gold test_bag_rules (test_bag_rules !! 3))
  test "shiny gold" False (contains_shiny_gold test_bag_rules (test_bag_rules !! 4))
  test "dark olive" False (contains_shiny_gold test_bag_rules (test_bag_rules !! 5))
  test "vibrant plum" False (contains_shiny_gold test_bag_rules (test_bag_rules !! 6))
  test "faded blue" False (contains_shiny_gold test_bag_rules (test_bag_rules !! 7))
  test "dotted black" False (contains_shiny_gold test_bag_rules (test_bag_rules !! 8))

  test "shiny gold containers" 4 (count_shiny_golds test_bag_rules)

  putStrLn "-- Part 1 Solution"

  let bag_rules = parse_bag_rules input
  putStrLn $ "Total bag colours (eventually) containing shiny gold: " ++ show (count_shiny_golds bag_rules)

  putStrLn "-- Part 2 Tests"

  let faded_blue_rule = colour_2_bag_rule test_bag_rules "faded blue"
  test "faded blue nesting count" 0 (nesting_count test_bag_rules (sub_bags faded_blue_rule))

  let vibrant_plum_rule = colour_2_bag_rule test_bag_rules "vibrant plum"
  test "vibrant plum nesting count" 11 (nesting_count test_bag_rules (sub_bags vibrant_plum_rule))

  let dark_olive_rule = colour_2_bag_rule test_bag_rules "dark olive"
  test "dark olive nesting count" 7 (nesting_count test_bag_rules (sub_bags dark_olive_rule))

  let test_shiny_gold_rule = colour_2_bag_rule test_bag_rules shiny_gold
  test "shiny gold nesting count" 32 (nesting_count test_bag_rules (sub_bags test_shiny_gold_rule))

  putStrLn "-- Part 2 Solution"

  let shiny_gold_rule = colour_2_bag_rule bag_rules shiny_gold
  let shiny_gold_nesting_count = nesting_count bag_rules (sub_bags shiny_gold_rule)
  putStrLn $ "Shiny gold nesting count: " ++ show shiny_gold_nesting_count
