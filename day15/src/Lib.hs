module Lib
    ( part1
    , part2
    ) where

import Text.Parsec.String (Parser)
import Text.Parsec (char, option, many, string, sepBy, parse)
import Text.Parsec.Char (digit, letter)
import Text.Parsec.Combinator (choice, many1)
import Text.Parsec.Prim (try)
import Data.List (transpose)

parseNumber :: Parser Integer
parseNumber = do
                sign <- option "" $ string "-"
                digits <- many1 digit
                return $ read (sign ++ digits)

parseIngedient :: Parser [Integer]
parseIngedient = do
                   name <- many1 letter
                   string ": capacity "
                   capacity <- parseNumber
                   string ", durability "
                   durability <- parseNumber
                   string ", flavor "
                   flavor <- parseNumber
                   string ", texture "
                   texture <- parseNumber
                   string ", calories "
                   calories <- parseNumber
                   return $ [capacity, durability, flavor, texture, calories]

amounts _ 0 = []
amounts 0 m = [take m $ repeat 0]
amounts n 1 = [[n]]
amounts n m = do
                x <- [0..n]
                xs <- amounts (n - x) (m - 1)
                return (x:xs)

part1 :: String -> String
part1 input = show . maximum . map (product . map (max 0) . take 4 . cookie) $ amounts 100 $ length ingredients
    where ingredients = lines input
          properties = transpose . map (either undefined id . parse parseIngedient "") $ ingredients
          cookie xs = map (sum . zipWith (*) xs) properties

calories500 xs = (head $ reverse xs) == 500

part2 :: String -> String
part2 input = show . maximum . map (product . map (max 0) . take 4) . filter calories500 . map cookie $ amounts 100 $ length ingredients
    where ingredients = lines input
          properties = transpose . map (either undefined id . parse parseIngedient "") $ ingredients
          cookie xs = map (sum . zipWith (*) xs) properties
