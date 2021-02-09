module Lib
    ( part1
    , part2
    ) where

import Text.Parsec.String (Parser)
import Text.Parsec (char, string, parse)
import Text.Parsec.Char (hexDigit, letter)
import Text.Parsec.Combinator (choice, count, many1)
import Text.Parsec.Prim (try)
import Data.Char (chr)
import Numeric (readHex)

parseEscapedAscii :: Parser (Char, Int, Int)
parseEscapedAscii = do
             string "\\x"
             ascii <- count 2 hexDigit
             return (chr . fst . head . readHex $ ascii, 1, 4)

parseEscapedDoublequote :: Parser (Char, Int, Int)
parseEscapedDoublequote = do
              string "\\\""
              return ('"', 1, 2)

parseEscapedBackslash :: Parser (Char, Int, Int)
parseEscapedBackslash = do
              string "\\\\"
              return ('\\', 1, 2)

parseChar :: Parser (Char, Int, Int)
parseChar = do
              c <- letter
              return (c, 1, 1)

parseChars :: Parser [(Char, Int, Int)]
parseChars = do
              char '"'
              chars <- many1 $ choice $ map try [parseEscapedAscii, parseEscapedDoublequote, parseEscapedBackslash, parseChar]
              char '"'
              return chars

parseEmpty :: Parser [(Char, Int, Int)]
parseEmpty = do
               string "\"\""
               return []

parseString :: Parser [(Char, Int, Int)]
parseString = choice $ map try [parseChars, parseEmpty]

overhead :: [(Char, Int, Int)] -> Int
overhead chars = code chars - memory chars
    where first (value, _, _) = value
          second (_, value, _) = value
          third (_, _, value) = value
          memory chars = sum (map second chars)
          code chars = 2 + sum (map third chars)

part1 :: String -> String
-- part1 = show . map (either undefined id . parse parseString "") . lines
part1 = show . sum . map (overhead . either undefined id . parse parseString "") . lines

part2 :: String -> String
part2 = const ""
