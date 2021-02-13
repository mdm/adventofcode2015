module Lib
    ( part1
    , part2
    ) where

import Text.Parsec.String (Parser)
import Text.Parsec (char, option, many, string, sepBy, parse)
import Text.Parsec.Char (digit, letter)
import Text.Parsec.Combinator (choice, many1)
import Text.Parsec.Prim (try)
import Data.Char(isDigit)
import Data.Maybe (mapMaybe)

data JSON = String String | Number Int | Array [JSON] | Object [(String, JSON)]

isOther x = isDigit x || x == '-'

parseProperty :: Parser (String, JSON)
parseProperty = do
                  char '"'
                  name <- many letter
                  string "\":"
                  value <- parseJSON
                  return (name, value)

parseObject :: Parser JSON
parseObject = do
                char '{'
                properties <- sepBy parseProperty (char ',')
                char '}'
                return $ Object properties

parseArray :: Parser JSON
parseArray = do
                char '['
                elements <- sepBy parseJSON (char ',')
                char ']'
                return $ Array elements

parseString :: Parser JSON
parseString = do
                char '"'
                letters <- many letter
                char '"'
                return $ String letters

parseNumber :: Parser JSON
parseNumber = do
                sign <- option "" $ string "-"
                digits <- many1 digit
                return $ Number $ read (sign ++ digits)

parseJSON :: Parser JSON
parseJSON = choice $ map try [parseObject, parseArray, parseString, parseNumber]

allNumbers :: JSON -> [Int]
allNumbers (String _) = []
allNumbers (Number x) = [x]
allNumbers (Array xs) = concatMap allNumbers xs
allNumbers (Object ps) = concatMap (allNumbers . snd) ps

maybeString :: JSON -> Maybe String
maybeString (String xs) = Just xs
maybeString _ = Nothing

nonRedNumbers :: JSON -> [Int]
nonRedNumbers (String _) = []
nonRedNumbers (Number x) = [x]
nonRedNumbers (Array xs) = concatMap allNumbers xs
nonRedNumbers (Object ps) | elem "red" $ mapMaybe (maybeString . snd) ps = []
                          | otherwise = concatMap (allNumbers . snd) ps

part1 :: String -> String
part1 = show . sum . allNumbers . either undefined id . parse parseJSON ""

part2 :: String -> String
part2 = show . sum . nonRedNumbers . either undefined id . parse parseJSON ""
