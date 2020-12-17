import AoC
import Data.List

input = readDayInputLines 7

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

shinyGold = "shiny gold"

parseBagCount :: String -> BagCount
parseBagCount bag_count_str = do
  let useful = head (splitStr " bag" bag_count_str)
  let str_words = words useful
  let bag_cnt = str2int (head str_words)
  let bag_clr = unwords (tail str_words)
  BagCount { count = bag_cnt, bag_colour = bag_clr }

parseBagCounts :: String -> [BagCount]
parseBagCounts "no other bags." = [] -- no BagCounts
parseBagCounts bag_counts_str = do
  let bag_count_strings = splitStr ", " bag_counts_str
  map parseBagCount bag_count_strings

parseBagRule :: String -> BagRule
parseBagRule rule = do
  let rule_split = splitStr " bags contain " rule
  let colour = head rule_split
  let bag_counts = parseBagCounts (rule_split !! 1)
  BagRule { rule_colour = colour, sub_bags = bag_counts }

parseBagRules :: [String] -> [BagRule]
parseBagRules rules_list = do
  map parseBagRule rules_list

colourMatch :: String -> BagRule -> Bool
colourMatch colour bag_rule = do
  rule_colour bag_rule == colour

colour2BagRule :: [BagRule] -> String -> BagRule
colour2BagRule bag_rules clr = do
  head (filter (colourMatch clr) bag_rules)

isShinyGold :: BagCount -> Bool
isShinyGold bag_count = do
  bag_colour bag_count == shinyGold

containsShinyGold :: [BagRule] -> BagRule -> Bool
containsShinyGold all_bag_rules bag_rule = do
  let sub_bag_counts = sub_bags bag_rule
  any isShinyGold sub_bag_counts
    ||
      (do
        not (null sub_bag_counts)
          &&
            (do let sub_bag_colours = map bag_colour sub_bag_counts
                let sub_bag_rules = map (colour2BagRule all_bag_rules) sub_bag_colours
                -- Recursively check 'child' bag rules for gold!
                any (containsShinyGold all_bag_rules) sub_bag_rules))

countShinyGolds :: [BagRule] -> Int
countShinyGolds bag_rules = do
  length (filter (containsShinyGold bag_rules) bag_rules)

nestingCountSingle :: [BagRule] -> BagCount -> Int
nestingCountSingle all_bag_rules bag_count = do
  let cnt = count bag_count
  let bag_col = bag_colour bag_count
  let bag_rule = colour2BagRule all_bag_rules bag_col
  let bags = sub_bags bag_rule
  -- Count child bags, plus (count * number of each of their children)
  cnt + cnt * nestingCount all_bag_rules bags

nestingCount :: [BagRule] -> [BagCount] -> Int
nestingCount all_bag_rules bag_counts = do
  sum (map (nestingCountSingle all_bag_rules) bag_counts)

main = do
  putStrLn "AoC 2020 - Day 7"

  putStrLn "-- Part 1 Tests"

  let test_bag_count1 = BagCount { count = 5, bag_colour = "faded blue" }
  let test_bag_count2 = BagCount { count = 1, bag_colour = "shiny maroon" }
  test "parse bag count A" test_bag_count1 (parseBagCount "5 faded blue bags,")
  test "parse bag count B" test_bag_count2 (parseBagCount "1 shiny maroon bag.")

  let test_bag_counts = [test_bag_count1, test_bag_count2]
  test "parse bag counts" test_bag_counts (parseBagCounts "5 faded blue bags, 1 shiny maroon bag.")

  let test_bag_rule_str = "clear lavender bags contain 5 faded blue bags, 1 shiny maroon bag."
  let test_bag_rule = BagRule { rule_colour = "clear lavender", sub_bags = test_bag_counts }
  test "parse bag rule" test_bag_rule (parseBagRule test_bag_rule_str)

  test "parse bag rules" [test_bag_rule] (parseBagRules [test_bag_rule_str])

  let test_bag_rules = parseBagRules test_input

  test "light red" True (containsShinyGold test_bag_rules (head test_bag_rules))
  test "dark orange" True (containsShinyGold test_bag_rules (test_bag_rules !! 1))
  test "bright white" True (containsShinyGold test_bag_rules (test_bag_rules !! 2))
  test "muted yellow" True (containsShinyGold test_bag_rules (test_bag_rules !! 3))
  test "shiny gold" False (containsShinyGold test_bag_rules (test_bag_rules !! 4))
  test "dark olive" False (containsShinyGold test_bag_rules (test_bag_rules !! 5))
  test "vibrant plum" False (containsShinyGold test_bag_rules (test_bag_rules !! 6))
  test "faded blue" False (containsShinyGold test_bag_rules (test_bag_rules !! 7))
  test "dotted black" False (containsShinyGold test_bag_rules (test_bag_rules !! 8))

  test "shiny gold containers" 4 (countShinyGolds test_bag_rules)

  putStrLn "-- Part 1 Solution"

  let bag_rules = parseBagRules input
  putStrLn $ "Total bag colours (eventually) containing shiny gold: " ++ show (countShinyGolds bag_rules)

  putStrLn "-- Part 2 Tests"

  let faded_blue_rule = colour2BagRule test_bag_rules "faded blue"
  test "faded blue nesting count" 0 (nestingCount test_bag_rules (sub_bags faded_blue_rule))

  let vibrant_plum_rule = colour2BagRule test_bag_rules "vibrant plum"
  test "vibrant plum nesting count" 11 (nestingCount test_bag_rules (sub_bags vibrant_plum_rule))

  let dark_olive_rule = colour2BagRule test_bag_rules "dark olive"
  test "dark olive nesting count" 7 (nestingCount test_bag_rules (sub_bags dark_olive_rule))

  let test_shiny_gold_rule = colour2BagRule test_bag_rules shinyGold
  test "shiny gold nesting count" 32 (nestingCount test_bag_rules (sub_bags test_shiny_gold_rule))

  putStrLn "-- Part 2 Solution"

  let shiny_gold_rule = colour2BagRule bag_rules shinyGold
  let shiny_gold_nestingCount = nestingCount bag_rules (sub_bags shiny_gold_rule)
  putStrLn $ "Shiny gold nesting count: " ++ show shiny_gold_nestingCount
