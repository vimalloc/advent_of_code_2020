import Data.Maybe
import Data.List
import Data.List.Split
import System.IO

validFields :: [String]
validFields = [ "byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid", "cid" ]

requiredFields :: [String]
requiredFields = sort $ filter (/= "cid") validFields

fieldIsValid :: String -> Bool
fieldIsValid field = elem field validFields

passportIsValid :: [String] -> Bool
passportIsValid xs = containsOnlyValidFields && containsAllRequiredFields
  where
    fields = sort . map (takeWhile (/= ':')) $ xs
    containsOnlyValidFields = all id . map fieldIsValid $ fields
    containsAllRequiredFields = requiredFields == intersect fields requiredFields

parsePassports :: String -> [[String]]
parsePassports = map words . splitOn "\n\n"

main = do
  fh <- openFile "input.txt" ReadMode
  input <- hGetContents fh
  print . length . filter id . map passportIsValid . parsePassports $ input
  hClose fh
