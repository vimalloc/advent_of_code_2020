import Data.List.Split
import System.IO
import Text.Parsec
import Text.ParserCombinators.Parsec.Number
import Text.Parsec.String (Parser)
import qualified Data.Set as Set

testInput :: String
testInput = "nop +0\nacc +1\njmp +4\nacc +3\njmp -3\nacc -99\nacc +1\njmp -4\nacc +6"

type Operation = (String, Int)

-- TODO: Instead of parsing on each line, lets parse the whole thing in one go.
parseInput :: String -> Either ParseError [Operation]
parseInput = traverse (parse parseLine "testInput") . splitOn "\n"
  where
    parseLine = do
      operation <- try (string "nop") <|> try (string "acc") <|> try (string "jmp")
      space
      value <- int
      return (operation, value)

solve :: [String] -> Int
solve x = solve' 0 0 (Set.fromList []) 
  where
    solve' :: Int -> Int -> Set.Set String -> Int
    solve' index counter seen = 0
      where
        foo = "bar"

main = do
  print testInput
  case (parseInput testInput) of
    Right operations -> print operations
    Left error       -> print error
