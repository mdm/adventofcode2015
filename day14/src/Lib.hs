module Lib
    ( part1
    , part2
    ) where

import Text.Parsec.String (Parser)
import Text.Parsec (char, option, many, string, sepBy, parse)
import Text.Parsec.Char (digit, letter)
import Text.Parsec.Combinator (choice, many1)
import Text.Parsec.Prim (try)

data Reindeer = Reindeer String Int Int Int
    deriving Show

parseReindeer :: Parser Reindeer
parseReindeer = do
                  name <- many1 letter
                  string " can fly "
                  speed <- many1 digit
                  string " km/s for "
                  stamina <- many1 digit
                  string " seconds, but then must rest for "
                  rest <- many1 digit
                  string " seconds."
                  return $ Reindeer name (read speed) (read stamina) (read rest)

distance seconds r@(Reindeer _ speed stamina rest) | seconds >= stamina = stamina * speed + distance (seconds - stamina - rest) r
                                                   | seconds > 0 = seconds * speed
                                                   | otherwise = 0

part1 :: String -> String
part1 = show . maximum . map (distance 2503 . either undefined id . parse parseReindeer "") . lines

score seconds rs = foldl update zeros [1..seconds]
    where zeros = take (length rs) (repeat 0)
          increments xs = map (\x -> if x == maximum xs then 1 else 0) xs
          update ss t = zipWith (+) ss $ increments $ map (distance t) rs

part2 :: String -> String
part2 = show . maximum . score 2503 . map (either undefined id . parse parseReindeer "") . lines
