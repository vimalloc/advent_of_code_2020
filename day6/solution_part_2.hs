import Data.List
import Data.List.Split
import System.IO

main = print . sum . map (length . foldr1 intersect . words) . splitOn "\n\n" =<< readFile "input.txt"
