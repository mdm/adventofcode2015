module Lib
    ( part1
    , part2
    ) where

import Debug.Trace(trace)
import Data.List (sortBy, minimumBy)


target1 :: [Int] -> Int
target1 xs = sum xs `div` 3

target2 :: [Int] -> Int
target2 xs = sum xs `div` 4

groups :: [Int] -> Int -> Int -> [Int] -> [[Int]]
groups taken size target [] | size == target = [taken]
                            | otherwise = []
groups taken size target (x:xs) | size > target = []
                                | size == target = [taken]
                                | otherwise = groups (x:taken) (size + x) target xs ++ groups taken size target xs

fewest :: [[Int]] -> [[Int]]
fewest xss = filter (\xs -> length xs == length (head sorted)) sorted
    where sorted = sortBy (\as bs -> compare (length as) (length bs)) xss

minQE :: [[Int]] -> [Int]
minQE = minimumBy (\as bs -> compare (product as) (product bs))

part1 :: String -> String
part1 = show . product . minQE . fewest . (\xs -> groups [] 0 (target1 xs) xs) . map read . lines

part2 :: String -> String
part2 = show . product . minQE . fewest . (\xs -> groups [] 0 (target2 xs) xs) . map read . lines
