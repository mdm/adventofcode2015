module Lib
    ( part1
    , part2
    ) where

import Text.Parsec.String (Parser)
import Text.Parsec (string, parse)
import Text.Parsec.Char (digit)
import Text.Parsec.Combinator (many1)
import Debug.Trace(trace)

parseCoords :: Parser (Int, Int)
parseCoords = do
                string "To continue, please consult the code grid in the manual.  Enter the code at row "
                y <- many1 digit
                string ", column "
                x <- many1 digit
                return (read x, read y)

code :: Int -> Int -> Int
code 1 1 = 20151125
code x y | x == 0 = code (y - 2) 1
         | otherwise = (code (x - 1) (y + 1) * 252533) `mod` 33554393

part1 :: String -> String
part1 = show . uncurry code . either undefined id . parse parseCoords ""


part2 :: String -> String
part2 = const "There is no part 2 for this puzzle."
