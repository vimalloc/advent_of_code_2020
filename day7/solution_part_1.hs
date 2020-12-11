import Data.List
import Data.List.Split
import Data.Maybe (fromJust)
import System.IO

type BagColor = String

data BagRule = BagRule
  { color :: BagColor
  , contains :: [(BagColor, Int)]
  } deriving (Show)

parseInput :: String -> [BagRule]
parseInput = map parseLine . lines

parseLine :: String -> BagRule
parseLine xs = BagRule color $ map parseContainStr containStrs
  where
    color = head $ splitOn " bags " xs
    containStrs = filter (not . isInfixOf "no other bags") . splitOn ", " . head . tail . splitOn "contain " $ xs

parseContainStr :: String -> (BagColor, Int)
parseContainStr xs = (color, number)
  where
    number = read . head . splitOn " " $ xs :: Int
    color = tail . dropWhile (/= ' ') . head . splitOn " bag" $ xs

getBag :: [BagRule] -> BagColor -> BagRule
getBag rules target = fromJust $ find (\br -> color br == target) rules

canContainGoldBag :: [BagRule] -> BagRule -> Bool
canContainGoldBag bagRules bag
  | childColors == []             = False
  | elem "shiny gold" childColors = True
  | otherwise                     = or . map (canContainGoldBag bagRules) . map (getBag bagRules) $ childColors
  where
    childColors = map fst . contains $ bag

-- TODO: This would greatly benefit from a memolizer so I'm not constantantly 
--       recomputing data. Using a Data.Map for the bag rules would be a better
--       choice too. However it runs in about ~10 seconds and give me the right
--       answer so I'm calling it good enough ¯\_(ツ)_/¯ 
solve :: String -> Int
solve xs = length . filter id . map (canContainGoldBag bagRules) $ bagRules
  where
    bagRules = parseInput xs

main  = do
  fh <- openFile "input.txt" ReadMode
  input <- hGetContents fh
  print . solve $ input
  hClose fh
