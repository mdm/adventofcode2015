module Lib
    ( part1
    , part2
    ) where

import Text.Parsec.String (Parser)
import Text.Parsec (char, string, parse)
import Text.Parsec.Char (digit, letter)
import Text.Parsec.Combinator (choice, many1)
import Text.Parsec.Prim (try)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.List(nub, permutations)

parseDistance :: Parser ((String, String), Int)
parseDistance = do
             cityA <- many1 letter
             string " to "
             cityB <- many1 letter
             string " = "
             distance <- many1 digit
             return ((cityA, cityB), read distance)


part1 :: String -> String
part1 input = show . minimum . map (tour input) . permutations $ cities input
    where parsed = map (either undefined id . parse parseDistance "") . lines
          cities input = nub $ map (fst . fst) (parsed input) ++ map (snd . fst) (parsed input)
          insert ds ((cityA, cityB), d) = Map.insert (cityA, cityB) d $ Map.insert (cityB, cityA) d ds
          distanceMap input = foldl insert Map.empty $ parsed input
          tour input permutation = sum $ map ((Map.!) $ distanceMap input) (zip permutation $ tail permutation)

part2 :: String -> String
part2 input = show . maximum . map (tour input) . permutations $ cities input
    where parsed = map (either undefined id . parse parseDistance "") . lines
          cities input = nub $ map (fst . fst) (parsed input) ++ map (snd . fst) (parsed input)
          insert ds ((cityA, cityB), d) = Map.insert (cityA, cityB) d $ Map.insert (cityB, cityA) d ds
          distanceMap input = foldl insert Map.empty $ parsed input
          tour input permutation = sum $ map ((Map.!) $ distanceMap input) (zip permutation $ tail permutation)
