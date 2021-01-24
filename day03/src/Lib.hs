module Lib
    ( part1
    , part2
    ) where

import Data.List(nub)

translate :: Char -> (Int, Int)
translate '<' = (-1, 0)
translate '>' = (1, 0)
translate '^' = (0, -1)
translate 'v' = (0, 1)

move :: (Int, Int) -> (Int, Int) -> (Int, Int)
move (x, y) (a, b) = (x + a, y + b)

partition :: [a] -> ([a], [a])
partition = partition' [] []
    where partition' ys zs [] = (reverse ys, reverse zs)
          partition' ys zs (x:xs) = partition' (x:zs) ys xs

part1 :: String -> String
part1 = show . length . nub . scanl move (0, 0) . map translate

part2 :: String -> String
part2 = show . length . nub . combine . partition . map translate
    where combine (xs, ys) = scanl move (0, 0) xs ++ scanl move (0, 0) ys

