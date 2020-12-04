import System.IO

-- Take one line at a time, and return an infinite list of bools where
-- True means there is a tree, and False means there is no tree.
parseInput :: String -> [Bool]
parseInput = cycle . map (== '#')

solve :: Int -> Int -> Int -> Int -> [[Bool]] -> Int
solve xMove yMove x y slope
  | y >= length slope = 0 -- Base case, at bottom
  | slope !! y !! x   = 1 + solve xMove yMove (x + xMove) (y + yMove) slope -- At a tree
  | otherwise         = 0 + solve xMove yMove (x + xMove) (y + yMove) slope -- Not at a tree

main = do
  fh <- openFile "input.txt" ReadMode
  input <- hGetContents fh
  let parsedInput =  map parseInput $ lines input
  let howToMove = [(1, 1), (3, 1), (5, 1), (7, 1), (1, 2)]
  print . product . map (\m -> solve (fst m) (snd m) 0 0 parsedInput) $ howToMove 
  hClose fh
