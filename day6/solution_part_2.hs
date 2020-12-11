import Data.List
import Data.List.Split
import Data.Function
import System.IO

numAllAnsweredYes :: [String] -> Int
numAllAnsweredYes (x:xs) = length $ foldr (\x acc -> intersect x acc) x xs

main  = do
  fh <- openFile "input.txt" ReadMode
  input <- hGetContents fh
  print . sum . map numAllAnsweredYes . map words . splitOn "\n\n" $ input
  hClose fh
