module Lib
    ( part1
    , part2
    ) where

import Text.Parsec.String (Parser)
import Text.Parsec (char, string, parse)
import Text.Parsec.Char (digit, space)
import Text.Parsec.Combinator (choice, many1)
import Text.Parsec.Prim (try)
import Data.Array (Array, (//), (!), array, elems, range)

data Rectangle = Rectangle (Int, Int) (Int, Int)
    deriving Show

data Command = TurnOn Rectangle | TurnOff Rectangle | Toggle Rectangle
    deriving Show

parseInt :: Parser Int
parseInt = do
             s <- many1 digit
             return $ read s

parseRectangle :: Parser Rectangle
parseRectangle = do
                   x0 <- parseInt
                   char ','
                   y0 <- parseInt
                   string " through "
                   x1 <- parseInt
                   char ','
                   y1 <- parseInt
                   return $ Rectangle (x0, y0) (x1, y1)

parseTurnOn :: Parser (Rectangle -> Command)
parseTurnOn = do
                string "turn on"
                return TurnOn

parseTurnOff :: Parser (Rectangle -> Command)
parseTurnOff = do
                string "turn off"
                return TurnOff

parseToggle :: Parser (Rectangle -> Command)
parseToggle = do
                string "toggle"
                return Toggle

parseCommand :: Parser Command
parseCommand = do
                 a <- choice $ map try [parseTurnOn, parseTurnOff, parseToggle]
                 space
                 r <- parseRectangle
                 return $ a r

lights (Rectangle topLeft bottomRight) = range (topLeft, bottomRight)

grid = array ((0, 0), (999, 999)) [(i, 0) | i <- range ((0, 0), (999, 999))]

execute1 :: Array (Int, Int) Int -> Command -> Array (Int, Int) Int
execute1 xs (TurnOn r) = xs // [(i, 1) | i <- lights r]
execute1 xs (TurnOff r) = xs // [(i, 0) | i <- lights r]
execute1 xs (Toggle r) = xs // [(i, 1 - xs!i) | i <- lights r]

part1 :: String -> String
part1 = show . length . filter (/=0) . elems . foldl execute1 grid . map (either undefined id . parse parseCommand "") . lines

execute2 :: Array (Int, Int) Int -> Command -> Array (Int, Int) Int
execute2 xs (TurnOn r) = xs // [(i, xs!i + 1) | i <- lights r]
execute2 xs (TurnOff r) = xs // [(i, max 0 $ xs!i - 1) | i <- lights r]
execute2 xs (Toggle r) = xs // [(i, xs!i + 2) | i <- lights r]

part2 :: String -> String
part2 = show . sum . elems . foldl execute2 grid . map (either undefined id . parse parseCommand "") . lines
