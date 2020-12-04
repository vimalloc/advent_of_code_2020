import System.IO
import Data.List

data PasswordPolicy = PasswordPolicy
    { requiredLetter :: !Char
    , possibleIndex1 :: !Int
    , possibleIndex2 :: !Int
    , password       :: !String
    } deriving (Show)

-- TODO should probabably use a regex here, but then I need to figure out how
--      to create a sandbox and install that package so ¯\_(ツ)_/¯
parseInput :: String -> PasswordPolicy
parseInput s = PasswordPolicy requiredLetter possibleIndex1 possibleIndex2 password
  where
    possibleIndex1 = read . takeWhile (/= '-') $ s :: Int
    possibleIndex2 = read . takeWhile (/= ' ') . tail . dropWhile (/= '-') $ s :: Int
    requiredLetter = head . tail . dropWhile (/= ' ') $ s
    password = tail . tail . dropWhile (/= ':') $ s

passwordValid :: PasswordPolicy -> Bool
passwordValid (PasswordPolicy c i1 i2 p) = (i1IsChar && not i2IsChar) || (not i1IsChar && i2IsChar)
  where
    charIndicies = map (+1) $ elemIndices c p
    i1IsChar = elem i1 charIndicies
    i2IsChar = elem i2 charIndicies

solve :: String -> Int
solve = length . filter id . map passwordValid . map parseInput . lines
  
main = do
  fh <- openFile "input.txt" ReadMode
  input <- hGetContents fh
  print $ solve input
  hClose fh
