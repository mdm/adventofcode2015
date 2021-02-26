module Lib
    ( part1
    , part2
    ) where

import Data.Array (Ix, Array, array, range, (!))

tabulate :: Ix i => (i -> e) -> (i, i) -> Array i e
tabulate f bounds = array bounds [(x, f x) | x <- range bounds]

combinations :: Int -> [Int] -> Integer 
combinations 0 _ = 1
combinations _ [] = 0
combinations n (x:xs) = combinations n xs + combinations (n - x) xs

part1 :: String -> String
part1 = show . combinations 150 . map read . lines

containers acc 0 _ = [acc]
containers _ _ [] = []
containers acc n (x:xs) = containers acc n xs ++ containers (x:acc) (n - x) xs

part2 :: String -> String
part2 input = show . length . filter (==minLength) $ lengths
    where lengths = map length . containers [] 150 . map read . lines $ input
          minLength = minimum lengths
