import System.IO

data PasswordPolicy = PasswordPolicy
    { requiredLetter :: !Char
    , minOccurrences :: !Int
    , maxOccurrences :: !Int
    , password       :: !String
    } deriving (Show)

-- TODO should probabably use a regex here, but then I need to figure out how
--      to create a sandbox and install that package so ¯\_(ツ)_/¯
parseInput :: String -> PasswordPolicy
parseInput s = PasswordPolicy requiredLetter minOccurrences maxOccurrences password
  where
    minOccurrences = read . takeWhile (/= '-') $ s :: Int
    maxOccurrences = read . takeWhile (/= ' ') . tail . dropWhile (/= '-') $ s :: Int
    requiredLetter = head . tail . dropWhile (/= ' ') $ s
    password = tail . tail . dropWhile (/= ':') $ s

passwordValid :: PasswordPolicy -> Bool
passwordValid (PasswordPolicy c min max p) = numChars >= min && numChars <= max
  where
    numChars = length . filter id . map (== c) $ p

solve :: String -> Int
solve = length . filter id . map passwordValid . map parseInput . lines
  
main = do
  fh <- openFile "input.txt" ReadMode
  input <- hGetContents fh
  print $ solve input
  hClose fh
