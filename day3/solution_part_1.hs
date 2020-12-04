import System.IO

-- Take one line at a time, and return an infinite list of bools where
-- True means there is a tree, and False means there is no tree.
parseInput :: String -> [Bool]
parseInput = cycle . map (== '#')

solve :: Int -> Int -> [[Bool]] -> Int
solve y x slope
  | y >= length slope = 0                           -- Base case, at bottom
  | slope !! y !! x   = 1 + solve (y+1) (x+3) slope -- Index is a tree
  | otherwise         = 0 + solve (y+1) (x+3) slope -- Index is not a tree

main = do
  fh <- openFile "input.txt" ReadMode
  input <- hGetContents fh
  print . solve 0 0 . map parseInput $ lines input
  hClose fh
