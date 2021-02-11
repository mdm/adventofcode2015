module Lib
    ( part1
    , part2
    ) where

import Data.Char(ord)

increment:: String -> String
increment = reverse . increment' . reverse
    where increment' [] = []
          increment' (x:xs) | x == 'z' = 'a':increment' xs
                            | otherwise = succ x : xs

valid xs = straight xs && noForbidden xs && twoPairs xs
    where straight xs = any (\(x, y, z) -> (ord y - ord x == 1) && (ord z - ord y == 1)) $ zip3 xs (tail xs) (tail $ tail xs)
          noForbidden xs = not (elem 'i' xs || elem 'o' xs ||elem 'l' xs)
          pairs xs = filter (\(_, x, y) -> x == y) $ zip3 [0..] xs (tail xs)
          twoPairs xs = not $ null [() | (i, _, _) <- pairs xs, (j, _, _) <- pairs xs, abs (i - j) >= 2]

part1 :: String -> String
part1 = head . filter valid . iterate increment . head . lines

part2 :: String -> String
part2 = head . tail . filter valid . iterate increment . head . lines
