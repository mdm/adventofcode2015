module Lib
    ( part1
    , part2
    ) where

rle (x:xs) = rle' 1 x xs
    where rle' run x [] = [(run, x)]
          rle' run x (y:ys) | x == y = rle' (run + 1) x ys
                               | otherwise = (run, x):rle' 1 y ys

assemble [] = []
assemble ((run, x):ys) = show run ++ [x] ++ assemble ys

part1 :: String -> String
part1 = show . length . (!!40) . iterate (assemble . rle) . head . lines

part2 :: String -> String
part2 = show . length . (!!50) . iterate (assemble . rle) . head . lines
