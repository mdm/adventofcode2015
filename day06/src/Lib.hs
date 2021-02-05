module Lib
    ( part1
    , part2
    ) where

import Text.Parsec.String (Parser)
import Text.Parsec (char, string, parse)
import Text.Parsec.Char (digit, space)
import Text.Parsec.Combinator (choice, many1)
import Text.Parsec.Prim (try)
import Data.Set (Set, empty, fromList, intersection, union, (\\), partition, member)

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

lights (Rectangle (x0, y0) (x1, y1)) = fromList [(x, y) | y <- [y0..y1], x <- [x0..x1]]

execute :: Set (Int, Int) -> Command -> Set (Int, Int)
execute xs (TurnOn r) = xs `union` lights r
execute xs (TurnOff r) = xs \\ lights r
execute xs (Toggle r) = xs `union` off \\ on
    where (on, off) = partition (`member` xs) $ lights r

part1 :: String -> String
part1 = show . length . foldl execute empty . map (either undefined id . parse parseCommand "") . lines

part2 :: String -> String
part2 = const ""
