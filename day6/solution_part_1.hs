import Data.List
import Data.List.Split
import System.IO

main = 
  print . sum . map (length . nub . concat . words) . splitOn "\n\n" =<< hGetContents =<< openFile "input.txt" ReadMode
