module Lib
    ( part1
    , part2
    ) where

import Text.Parsec.String (Parser)
import Text.Parsec (char, string, parse, try)
import Text.Parsec.Char (digit)
import Text.Parsec.Combinator (choice, many1)
import Debug.Trace(trace)
import Data.Maybe (mapMaybe)
import Data.Array ((!), listArray, bounds, inRange, Array)

data Register = RegA | RegB
    deriving Show  
    
data Instruction = Hlf Register | Tpl Register | Inc Register | Jmp Int | Jie Register Int | Jio Register Int
    deriving Show  

parseRegA :: Parser Register
parseRegA = do
              char 'a'
              return RegA

parseRegB :: Parser Register
parseRegB = do
              char 'b'
              return RegB

parseHlf :: Parser Instruction
parseHlf = do
             string "hlf "
             reg <- choice $ map try [parseRegA, parseRegB]
             return $ Hlf reg

parseTpl :: Parser Instruction
parseTpl = do
             string "tpl "
             reg <- choice $ map try [parseRegA, parseRegB]
             return $ Tpl reg

parseInc :: Parser Instruction
parseInc = do
             string "inc "
             reg <- choice $ map try [parseRegA, parseRegB]
             return $ Inc reg

parsePositiveOffset :: Parser Int
parsePositiveOffset = do
                        char '+'
                        offset <- many1 digit 
                        return $ read offset

parseNegativeOffset :: Parser Int
parseNegativeOffset = do
                        char '-'
                        offset <- many1 digit 
                        return $ -read offset

parseJmp :: Parser Instruction
parseJmp = do
             string "jmp "
             offset <- choice $ map try [parsePositiveOffset, parseNegativeOffset]
             return $ Jmp offset

parseJie :: Parser Instruction
parseJie = do
             string "jie "
             reg <- choice $ map try [parseRegA, parseRegB]
             string ", "
             offset <- choice $ map try [parsePositiveOffset, parseNegativeOffset]
             return $ Jie reg offset

parseJio :: Parser Instruction
parseJio = do
             string "jio "
             reg <- choice $ map try [parseRegA, parseRegB]
             string ", "
             offset <- choice $ map try [parsePositiveOffset, parseNegativeOffset]
             return $ Jio reg offset

parseInstruction :: Parser Instruction
parseInstruction = do
                     choice $ map try [parseHlf, parseTpl, parseInc, parseJmp, parseJie, parseJio]

execute' :: Int -> Int -> Int -> Array Int Instruction -> Instruction -> Int
execute' pc a b prog (Hlf RegA) = execute (pc + 1) (a `div` 2) b prog
execute' pc a b prog (Hlf RegB) = execute (pc + 1) a (b `div` 2) prog
execute' pc a b prog (Tpl RegA) = execute (pc + 1) (a * 3) b prog
execute' pc a b prog (Tpl RegB) = execute (pc + 1) a (b * 3) prog
execute' pc a b prog (Inc RegA) = execute (pc + 1) (a + 1) b prog
execute' pc a b prog (Inc RegB) = execute (pc + 1) a (b + 1) prog
execute' pc a b prog (Jmp offset) = execute (pc + offset) a b prog
execute' pc a b prog (Jie RegA offset) = if even a then execute (pc + offset) a b prog else execute (pc + 1) a b prog
execute' pc a b prog (Jie RegB offset) = if even b then execute (pc + offset) a b prog else execute (pc + 1) a b prog
execute' pc a b prog (Jio RegA offset) = if a == 1 then execute (pc + offset) a b prog else execute (pc + 1) a b prog
execute' pc a b prog (Jio RegB offset) = if b == 1 then execute (pc + offset) a b prog else execute (pc + 1) a b prog

execute :: Int -> Int -> Int -> Array Int Instruction -> Int
execute pc a b prog | inRange (bounds prog) pc = {- trace (show pc ++ " " ++ show a ++ " " ++ show b ++ " " ++ show (prog ! pc)) -} execute' pc a b prog (prog ! pc)
                    | otherwise = b

makeArray xs = listArray (0, length xs - 1) xs

part1 :: String -> String
part1 = show . execute 0 0 0 . makeArray . map (either undefined id . parse parseInstruction "") . lines

part2 :: String -> String
part2 = show . execute 0 1 0 . makeArray . map (either undefined id . parse parseInstruction "") . lines
