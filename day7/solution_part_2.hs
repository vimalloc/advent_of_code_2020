import Data.List
import Data.List.Split
import Data.Maybe (fromJust)
import System.IO

testInput :: String
testInput = unlines [ "shiny gold bags contain 2 dark red bags.",
                      "dark red bags contain 2 dark orange bags.",
                      "dark orange bags contain 2 dark yellow bags",
                      "dark yellow bags contain 2 dark green bags.",
                      "dark green bags contain 2 dark blue bags.",
                      "dark blue bags contain 2 dark violet bags.",
                      "dark violet bags contain no other bags." ]

type BagColor = String

-- In hindsight this was not a very good data structure for this problem.
data BagRule = BagRule
  { color :: BagColor
  , children :: [(BagColor, Int)]
  } deriving (Show)

parseInput :: String -> [BagRule]
parseInput = map parseLine . lines

parseLine :: String -> BagRule
parseLine xs = BagRule color $ map parseChildrenStr childrenStrs
  where
    color = head $ splitOn " bags " xs
    childrenStrs = filter (not . isInfixOf "no other bags") . splitOn ", " . head . tail . splitOn "contain " $ xs

parseChildrenStr :: String -> (BagColor, Int)
parseChildrenStr xs = (color, number)
  where
    number = read . head . splitOn " " $ xs :: Int
    color = tail . dropWhile (/= ' ') . head . splitOn " bag" $ xs

getBag :: [BagRule] -> BagColor -> BagRule
getBag rules target = fromJust $ find (\br -> color br == target) rules

-- Holy hell this is so bad. Come back to it with a fresh brain later to make it pretty.
countChildren :: [BagRule] -> BagRule -> Int
countChildren rules bag = numDirectChildren + (sum . map (\c -> (snd c) * (countChildren rules (getBag rules (fst c)))) $ children bag)
  where
    numDirectChildren = sum . map snd $ children bag

solve :: String -> Int
solve xs = countChildren bagRules goldenBag
  where
    bagRules = parseInput xs
    goldenBag = getBag bagRules "shiny gold"

main  = do
  fh <- openFile "input.txt" ReadMode
  input <- hGetContents fh
  print . solve $ testInput
  hClose fh
