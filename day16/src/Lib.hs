module Lib
    ( part1
    , part2
    ) where

import Text.Parsec.String (Parser)
import Text.Parsec (char, option, many, string, sepBy, parse)
import Text.Parsec.Char (digit, letter)
import Text.Parsec.Combinator (choice, many1)
import Text.Parsec.Prim (try)
import Data.List (transpose)

parsePossession :: Parser (String, Int)
parsePossession = do
                    kind <- many1 letter
                    string ": "
                    number <- many1 digit
                    return (kind, read number)

parseSue :: Parser (Integer, [(String, Int)])
parseSue = do
             string "Sue "
             sue <- many1 digit
             string ": "
             possessions <- sepBy parsePossession (string ", ")
             return (read sue, possessions)

matchOne targets (possessionKind, possessionNumber) = all match targets
    where match (targetKind, targetNumber, targetOperator) = possessionKind /= targetKind || targetOperator possessionNumber targetNumber

matchAll targets (sue, possessions) = all (matchOne targets) possessions

targets1 = [("children", 3, (==)), ("cats", 7, (==)), ("samoyeds", 2, (==)), ("pomeranians", 3, (==)), ("akitas", 0, (==)), ("vizslas", 0, (==)), ("goldfish", 5, (==)), ("trees", 3, (==)), ("cars", 2, (==)), ("perfumes", 1, (==))]

part1 :: String -> String
part1 = show . fst . head . filter (matchAll targets1) . map (either undefined id . parse parseSue "") . lines

targets2 = [("children", 3, (==)), ("cats", 7, (>)), ("samoyeds", 2, (==)), ("pomeranians", 3, (<)), ("akitas", 0, (==)), ("vizslas", 0, (==)), ("goldfish", 5, (<)), ("trees", 3, (>)), ("cars", 2, (==)), ("perfumes", 1, (==))]

part2 :: String -> String
part2 = show . fst . head . filter (matchAll targets2) . map (either undefined id . parse parseSue "") . lines
