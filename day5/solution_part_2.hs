import System.IO
import Data.List

parseRow :: String -> Int
parseRow xs = head $ parseRow' xs [0..127]
  where
    parseRow' "" rows       = rows
    parseRow' ('F':xs) rows = parseRow' xs $ take (div (length rows) 2) rows
    parseRow' ('B':xs) rows = parseRow' xs $ drop (div (length rows) 2) rows


parseCol :: String -> Int
parseCol xs = head $ parseCol' xs [0..7]
  where
    parseCol' "" rows       = rows
    parseCol' ('L':xs) rows = parseCol' xs $ take (div (length rows) 2) rows
    parseCol' ('R':xs) rows = parseCol' xs $ drop (div (length rows) 2) rows

firstNotAtStart :: [Int] -> Int
firstNotAtStart xs = head $ firstNotAtStart' xs
  where
    firstNotAtStart' (x:y:ys)
      | x + 1 == y = firstNotAtStart' (y:ys)
      | otherwise  = [y]

solve :: String -> Int
solve xs = (row * 8) + col
  where
    row = parseRow $ take 7 xs
    col = parseCol $ drop 7 xs

main = do
  fh <- openFile "input.txt" ReadMode
  input <- hGetContents fh
  let allSeatIds = map solve . lines $ input
  let maxSeatId = maximum allSeatIds
  let missingSeats = [0..maxSeatId] \\ allSeatIds
  print $ firstNotAtStart missingSeats
  hClose fh
