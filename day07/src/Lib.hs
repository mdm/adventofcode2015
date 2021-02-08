module Lib
    ( part1
    , part2
    ) where

import Text.Parsec.String (Parser)
import Text.Parsec (char, string, parse)
import Text.Parsec.Char (digit, letter)
import Text.Parsec.Combinator (choice, many1)
import Text.Parsec.Prim (try)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Bits ((.&.), (.|.), complement, shiftL, shiftR)
import Data.Either (isLeft)

data Input = Const { value :: Int } | Wire { identifier :: String }
    deriving Show

data Instruction = Not Input | Forward Input | And Input Input | Or Input Input | LShift Input Int | RShift Input Int
    deriving Show

parseConst :: Parser Input
parseConst = do
             token <- many1 digit
             return $ Const $ read token

parseWire :: Parser Input
parseWire = do
              token <- many1 letter
              return $ Wire token

parseInput :: Parser Input
parseInput = do
               choice $ map try [parseConst, parseWire]

parseNot :: Parser Instruction
parseNot = do
             string "NOT "
             x <- parseInput
             return $ Not x

parseForward :: Parser Instruction
parseForward = do
                 x <- parseInput
                 return $ Forward x

parseAnd :: Parser Instruction
parseAnd = do
             x <- parseInput
             string " AND "
             y <- parseInput
             return $ And x y

parseOr :: Parser Instruction
parseOr = do
             x <- parseInput
             string " OR "
             y <- parseInput
             return $ Or x y

parseLShift :: Parser Instruction
parseLShift = do
             x <- parseInput
             string " LSHIFT "
             y <- parseConst
             return $ LShift x $ value y

parseRShift :: Parser Instruction
parseRShift = do
             x <- parseInput
             string " RSHIFT "
             y <- parseConst
             return $ RShift x $ value y

parseInstruction :: Parser (String, Instruction)
parseInstruction = do
                     i <- choice $ map try [parseNot, parseAnd, parseOr, parseLShift, parseRShift, parseForward]
                     string " -> "
                     w <- parseWire
                     return (identifier w, i)

resolve ([], resolved) = ([], resolved)
resolve (x:xs, resolved) | resolvable x resolved = (xs, insertResolution x restResolved)
                         | otherwise = (x:restUnresolved, restResolved)
    where (restUnresolved, restResolved) = resolve (xs, resolved)
          member' (Const a) _ = True
          member' (Wire x) resolved = Map.member x resolved
          resolvable (_, Not x) resolved = member' x resolved
          resolvable (_, Forward x) resolved = member' x resolved
          resolvable (_, And x y) resolved = member' x resolved && member' y resolved
          resolvable (_, Or x y) resolved = member' x resolved && member' y resolved
          resolvable (_, LShift x y) resolved = member' x resolved
          resolvable (_, RShift x y) resolved = member' x resolved
          insertResolution (w, i) resolved = Map.insert w (evaluate' i resolved)  resolved
          lookup' (Const a) _ = a
          lookup' (Wire x) resolved = resolved Map.! x
          evaluate' (Not x) resolved = (.&.) 0xffff . complement $ lookup' x resolved
          evaluate' (Forward x) resolved = lookup' x resolved
          evaluate' (And x y) resolved = lookup' x resolved .&. lookup' y resolved
          evaluate' (Or x y) resolved = lookup' x resolved .|. lookup' y resolved
          evaluate' (LShift x y) resolved = shiftL (lookup' x resolved) y
          evaluate' (RShift x y) resolved = shiftR (lookup' x resolved) y

evaluate :: [(String, Instruction)] -> Map String Int
evaluate is = snd $ until (null . fst) resolve (is, Map.empty)

part1 :: String -> String
part1 = show . flip (Map.!) "a" . evaluate . map (either undefined id . parse parseInstruction "") . lines
-- part1 = show . map (parse parseInstruction "") . lines


part2 :: String -> String
part2 = const ""
