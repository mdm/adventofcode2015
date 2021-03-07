module Lib
    ( part1
    , part2
    ) where

import Text.Parsec.String (Parser)
import Text.Parsec (string, parse)
import Text.Parsec.Char (letter)
import Text.Parsec.Combinator (many1)
import Data.List (nub, partition, isPrefixOf, isSubsequenceOf, tails, sortBy)
import Data.Tuple (swap)
import Data.Maybe (mapMaybe)
import Control.Monad (msum)


data Token = Terminal String | NonTerminal String
    deriving Show

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

nextToken :: [(String, String)] -> String -> (Token, String)
nextToken rules string = if null (terminalPrefix rules string) then wrapResult NonTerminal . head $ nonTerminalPrefixes rules string else wrapResult Terminal . terminalPrefix rules $ string
    where wrapResult constructor token = (constructor token, drop (length token) string)

terminalPrefix :: [(String, String)] -> String -> String
terminalPrefix rules [] = []
terminalPrefix rules string@(x:xs) | isTerminal rules string = x:terminalPrefix rules xs
                                   | otherwise = []

isTerminal :: [(String, String)] -> String -> Bool
isTerminal rules = null . nonTerminalPrefixes rules

nonTerminalPrefixes :: [(String, String)] -> String -> [String]
nonTerminalPrefixes rules xs = concatMap (extractNonTerminal xs) rules

extractNonTerminal :: String -> (String, String) -> [String]
extractNonTerminal xs (input, _) = [input | input `isPrefixOf` xs]

tokenize :: [(String, String)] -> String -> [Token]
tokenize rules [] = []
tokenize rules string = token:tokenize rules rest
    where (token, rest) = nextToken rules string

rhsLength:: (String, String) -> (String, String) -> Ordering 
rhsLength (_, a) (_, b) = compare (length b) (length a)

choice :: [(String, String)] -> [Token] -> [Token] -> Maybe Int
choice rules ((NonTerminal x):xs) ys | length xs < length ys = if null options then Nothing else Just $ 1 + head options
                                     | otherwise = Nothing
    where matchingRules = filter ((==x) . fst) rules
          applyRule (_, zs) = parseMedicine rules (tokenize rules zs ++ xs) ys
          options = mapMaybe applyRule matchingRules

parseMedicine :: [(String, String)] -> [Token] -> [Token] -> Maybe Int
parseMedicine rules [] ys = if null ys then Just 0 else Nothing
parseMedicine rules _ [] = Nothing
parseMedicine rules ((Terminal x):xs) ((Terminal y):ys) = if x == y then parseMedicine rules xs ys else Nothing
parseMedicine rules ((Terminal x):xs) ((NonTerminal y):ys) = Nothing
parseMedicine rules start@((NonTerminal x):xs) target@((Terminal y):ys) = choice rules start target
parseMedicine rules start@((NonTerminal x):xs) target@((NonTerminal y):ys) = msum [if x == y then parseMedicine rules xs ys else Nothing, choice rules start target]

part2 :: String -> String
part2 input = show $ parseMedicine (sortBy rhsLength parsedRules) (tokenize parsedRules "e") (tokenize parsedRules medicine)
-- part2 input = show $ sortBy rhsLength parsedRules
    where (medicine:_:rules) = reverse . lines $ input
          parsedRules = map (either undefined id . parse parseReplacement "") rules
