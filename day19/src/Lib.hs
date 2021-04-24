module Lib
    ( part1
    , part2
    ) where

import Text.Parsec.String (Parser)
import Text.Parsec (string, parse)
import Text.Parsec.Char (letter)
import Text.Parsec.Combinator (many1)
import Data.List (nub, isPrefixOf)
import Data.Char (isAsciiUpper)


parseReplacement :: Parser (String, String)
parseReplacement = do
                     input <- many1 letter
                     string " => "
                     output <- many1 letter
                     return (input, output)

replace :: String -> (String, String) -> [String]
replace [] _ = [[]]
replace start@(x:xs) rule@(input, output) | input `isPrefixOf` start = (output ++ rest):map (x:) (replace xs rule)
                                          | otherwise = map (x:) (replace xs rule)
    where rest = drop (length input) start

part1 :: String -> String
part1 input = show . length . nub . filter (/=start) . concatMap (replace start . either undefined id . parse parseReplacement "") $ rules
    where (start:_:rules) = reverse . lines $ input


prependAcc :: [a] -> [[a]] -> [[a]]
prependAcc acc xs = if null acc then xs else (reverse acc):xs

tokenize :: String -> String -> [String]
tokenize acc [] = prependAcc acc []
tokenize acc (x:xs) | isAsciiUpper x = prependAcc acc $ tokenize [x] xs
                    | otherwise = tokenize (x:acc) xs

minReductions :: Int -> Int -> Int -> [String] -> [String] -> Int
minReductions t r _ _ [] = t - 2 * r - 1
minReductions t r depth acc (x:xs) | x == "Rn" = if depth == 0 then minReductions (t + 1) (r + 1) (depth + 1) [] xs else minReductions t r (depth + 1) (x:acc) xs
                                   | depth == 1 && x == "Y" = minReductions 0 0 0 [] (reverse acc) + minReductions (t + 2) (r + 1) depth [] xs
                                   | x == "Ar" = if depth == 1 then minReductions 0 0 0 [] (reverse acc) + minReductions (t + 2) r (depth - 1) [] xs else minReductions t r (depth - 1) (x:acc) xs
                                   | otherwise = if depth == 0 then minReductions (t + 1) r depth acc xs else minReductions t r depth (x:acc) xs

part2 :: String -> String
part2 = show . minReductions 0 0 0 [] . tokenize "" . last . lines

