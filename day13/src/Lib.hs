module Lib
    ( part1
    , part2
    ) where

import Text.Parsec.String (Parser)
import Text.Parsec (char, option, many, string, sepBy, parse)
import Text.Parsec.Char (digit, letter)
import Text.Parsec.Combinator (choice, many1)
import Text.Parsec.Prim (try)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.List(nub, permutations)


parseGain :: Parser Int
parseGain = do
              string "gain "
              digits <- many1 digit
              return $ read digits

parseLoss :: Parser Int
parseLoss = do
              string "lose "
              digits <- many1 digit
              return $ -read digits

parseHappiness :: Parser ((String, String), Int)
parseHappiness = do
                   personA <- many1 letter
                   string " would "
                   change <- choice $ map try [parseGain, parseLoss]
                   string " happiness units by sitting next to "
                   personB <- many1 letter
                   return $ ((personA, personB), change)

neighbors :: [String] -> [(String, String)]
neighbors (x:xs) = zip (x:circular) circular
    where circular = xs ++ [x]

totalHapiness :: Map (String, String) Int -> [String] -> Int
totalHapiness cs = sum . map happiness . neighbors
    where happiness (personA, personB) = cs Map.! (personA, personB) + cs Map.! (personB, personA)

part1 :: String -> String
part1 input = show . maximum . map (totalHapiness changes) $ permutations people
    where parsed = map (either undefined id . parse parseHappiness "") $ lines input
          people = nub . map (fst . fst) $ parsed
          changes = foldl insert Map.empty $ parsed
          insert cs ((personA, personB), change) = Map.insert (personA, personB) change cs

part2 :: String -> String
part2 input = show . maximum . map (totalHapiness changes) $ permutations ("I":people)
    where parsed = map (either undefined id . parse parseHappiness "") $ lines input
          people = nub . map (fst . fst) $ parsed
          changes = foldl insert Map.empty $ parsed ++ myChanges
          insert cs ((personA, personB), change) = Map.insert (personA, personB) change cs
          myChanges = map (\p -> (("I", p), 0)) people ++ map (\p -> ((p, "I"), 0)) people
