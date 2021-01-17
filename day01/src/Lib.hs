module Lib
    ( part1
    , part2
    ) where

import Data.List(find)

translate :: Num p => Char -> p
translate '(' = 1
translate ')' = -1
translate _ = 0

part1 :: String -> String
part1 = show . sum . map translate

part2 :: String -> String
part2 = show . maybe undefined fst . find (\x -> snd x == -1) . zip [0..] . scanl (+) 0 . map translate
