module Lib
    ( part1
    , part2
    ) where

import Data.Text(pack, split, unpack)

parseLine :: String -> [Int]
parseLine = map (read . unpack) . split (=='x') . pack

parse :: String -> [[Int]]
parse = map parseLine . lines

sides :: [Int] -> [(Int, Int)]
sides xs = [(a, b) | (i, a) <- zip [0..] xs, (j, b) <- zip [0..] xs, i < j]

paper :: [Int] -> Int
paper xs = slack xs + (sum . map (*2) . areas) xs
    where slack = minimum . areas
          areas = map (uncurry (*)) . sides

ribbon :: [Int] -> Int
ribbon xs = bow xs + (minimum . perimeters) xs
    where bow = product
          perimeters = map ((*2) . uncurry (+)) . sides

part1 :: String -> String
part1 = show . sum . map paper . parse

part2 :: String -> String
part2 = show . sum . map ribbon . parse
