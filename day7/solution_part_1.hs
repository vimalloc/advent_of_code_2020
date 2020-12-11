import Data.List
import Data.List.Split
import Data.Function
import System.IO

testInput :: String
testInput = unlines [
  "light red bags contain 1 bright white bag, 2 muted yellow bags.",
  "dark orange bags contain 3 bright white bags, 4 muted yellow bags.",
  "bright white bags contain 1 shiny gold bag.",
  "muted yellow bags contain 2 shiny gold bags, 9 faded blue bags.",
  "shiny gold bags contain 1 dark olive bag, 2 vibrant plum bags.",
  "dark olive bags contain 3 faded blue bags, 4 dotted black bags.",
  "vibrant plum bags contain 5 faded blue bags, 6 dotted black bags.",
  "faded blue bags contain no other bags.",
  "dotted black bags contain no other bags." ]

type BagColor = String

data BagRule = BagRule
  { color :: BagColor
  , contains :: [(BagColor, Int)]
  } deriving (Show)

parseInput :: String -> [BagRule]
parseInput = map parseLine . lines

parseLine :: String -> BagRule
parseLine xs = BagRule "red" $ map parseContainStr containStrs
  where
    color = head $ splitOn "bags " xs
    containStrs = filter (not . isInfixOf "no other bags") . splitOn ", " . head . tail . splitOn "contain " $ xs

parseContainStr :: String -> (BagColor, Int)
parseContainStr xs = (color, number)
  where
    number = read . head . splitOn " " $ xs :: Int
    color = tail . dropWhile (/= ' ') . head . splitOn " bag" $ xs


-- Would like to have a memolized solution where as we go over each bag, we
-- can say if we alreaye know that it can contain the target color or not.
-- That would save a lot of computation.

main  = do
  fh <- openFile "input.txt" ReadMode
  input <- hGetContents fh
  print $ parseInput testInput
  hClose fh
