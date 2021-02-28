module Lib
    ( part1
    , part2
    ) where

import Text.Parsec.String (Parser)
import Text.Parsec (string, parse)
import Text.Parsec.Char (letter)
import Text.Parsec.Combinator (many1)
import Data.List (nub, isPrefixOf)
import Data.Tuple (swap)


parseReplacement :: Parser (String, String)
parseReplacement = do
                     input <- many1 letter
                     string " => "
                     output <- many1 letter
                     return (input, output)

replace :: String -> (String, String) -> [String]
replace [] _ = [[]]
replace start@(x:xs) rule@(input, output) | input `isPrefixOf` start = (output ++ rest):map (x:) (replace xs rule)
                                          | otherwise = map (x:) (replace xs rule)
    where rest = drop (length input) start

part1 :: String -> String
part1 input = show . length . nub . filter (/=start) . concatMap (replace start . either undefined id . parse parseReplacement "") $ rules
    where (start:_:rules) = reverse . lines $ input

part2 :: String -> String
part2 input = show . length . takeWhile (notElem "e") . iterate step $ [medicine]
-- part2 input = show . take 4 . iterate step $ ["e"]
    where (medicine:_:rules) = reverse . lines $ input
          parsedRules = map (swap . either undefined id . parse parseReplacement "") rules
          maxLen = length medicine
          step = concatMap (\x -> nub . filter (\y ->  y /= x && length y <= maxLen) . concatMap (replace x) $ parsedRules)
