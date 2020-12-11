import Data.List
import Data.List.Split
import System.IO

main =
  print . sum . map length . map (foldr1 intersect) . map words . splitOn "\n\n"  =<< hGetContents =<< openFile "input.txt" ReadMode
