import Data.Maybe
import Data.List
import Data.List.Split
import System.IO
import Text.Read
import Numeric

requiredFields :: [String]
requiredFields = [ "byr", "ecl", "eyr", "hcl", "hgt", "iyr", "pid" ]

validNumberString :: Int -> Int -> Int -> String -> Bool
validNumberString numDigits min max str
  | length str /= numDigits = False
  | otherwise = case (readMaybe str :: Maybe Int) of
                  Just int -> int >= min && int <= max
                  Nothing  -> False

fieldIsValid :: String -> Bool
fieldIsValid field
  | key == "byr" = validNumberString 4 1920 2002 value
  | key == "iyr" = validNumberString 4 2010 2020 value
  | key == "eyr" = validNumberString 4 2020 2030 value
  | key == "pid" = validNumberString 9 0 1000000000 value
  | key == "ecl" = elem value ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"]
  | key == "hcl" = length value == 7 && head value == '#' && (readHex (tail value)) /= []
  | key == "hgt" = case (drop (length value - 2) value) of
                     "cm"      -> validNumberString 3 150 193 $ take (length value - 2) value
                     "in"      -> validNumberString 2 59 76 $ take (length value - 2) value
                     otherwise -> False
  | key == "cid" = True
  | otherwise    = False
  where
    key = takeWhile (/= ':') field
    value = tail . dropWhile (/= ':') $ field

passportIsValid :: [String] -> Bool
passportIsValid fields = containsOnlyValidFields && containsAllRequiredFields
  where
    fieldKeys = sort . map (takeWhile (/= ':')) $ fields
    containsOnlyValidFields = all id . map fieldIsValid $ fields
    containsAllRequiredFields = requiredFields == intersect fieldKeys requiredFields

parsePassports :: String -> [[String]]
parsePassports = map words . splitOn "\n\n"

main = do
  fh <- openFile "input.txt" ReadMode
  input <- hGetContents fh
  print . length . filter id . map passportIsValid . parsePassports $ input
  hClose fh
