import Data.List.Split
import System.IO
import Text.Parsec
import Text.ParserCombinators.Parsec.Number
import Text.Parsec.String (Parser)
import qualified Data.Set as Set

type Operation = (String, Int)

-- TODO: How can I do this without having to go one line at a time, so that I
--       can preserve line number for errors (and remove that filter).
parseInput :: String -> Either ParseError [Operation]
parseInput = traverse (parse parseLine "testInput") . filter (/= "") . splitOn "\n"
  where
    parseLine = do
      operation <- try (string "nop") <|> try (string "acc") <|> try (string "jmp")
      space
      value <- int
      pure (operation, value)

solve :: [Operation] -> Int
solve operations = solve' 0 0 (Set.fromList []) 
  where
    solve' :: Int -> Int -> Set.Set Int -> Int
    solve' index counter seen
      | Set.member index seen = counter
      | operation == "nop"    = solve' (index + 1) counter $ Set.insert index seen
      | operation == "jmp"    = solve' (index + value) counter $ Set.insert index seen
      | operation == "acc"    = solve' (index + 1) (counter + value) (Set.insert index seen)
      where
        operation = fst $ operations !! index
        value = snd $ operations !! index

main = do
  input <- readFile "input.txt"
  case (parseInput input) of
    Right operations -> print . solve $ operations
    Left error       -> print error
