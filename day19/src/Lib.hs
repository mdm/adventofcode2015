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
import Debug.Trace (trace, traceShow)
import Data.Foldable (minimumBy, maximumBy)


data Token = Terminal String | NonTerminal String
    deriving (Show, Eq)

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

allReductions :: [(String, String)] -> [Token] -> [Token] -> [[Token]]
allReductions _ _ [] = []
allReductions _ _ [x] = []
allReductions rules acc ((NonTerminal x1):(NonTerminal x2):tokens) = map (\(lhs, rhs) -> acc ++ NonTerminal lhs:tokens) (matchingRules (x1 ++ x2)) ++ allReductions rules (acc ++ [NonTerminal x1]) (NonTerminal x2:tokens)
    where matchingRules rhs = filter ((==rhs) . snd) rules

parsePure :: [(String, String)] -> [Token] -> [(Int, Token)]
parsePure rules [x] = [(0, x)]
parsePure rules input = nub $ concatMap (map plusOne . parsePure rules) (allReductions rules [] input)
    where plusOne (depth, x) = (depth + 1, x)

withTerminals :: [Token] -> Bool
withTerminals [] = False
withTerminals ((Terminal x):xs) = True
withTerminals ((NonTerminal x):xs) = withTerminals xs

skipNested :: [Token] -> Int -> [Token] -> ([Token], [Token])
skipNested [] _ acc = (reverse acc, [])
skipNested input count acc | head input == Terminal "rA" = skipNested (tail input) (count + 1) ((head input):acc)
                           | head input == Terminal "nR" = if count == 0 then (reverse acc, tail input) else skipNested (tail input) (count - 1) ((head input):acc)
                           | otherwise = skipNested (tail input) count ((head input):acc)

partialInput :: [Token] -> ([Token], [Token])
partialInput [] = ([], [])
partialInput input | head input == Terminal "rA" = (skipped ++ partial, rest2)
                   | withTerminals [head input] = ([], input)
                   | otherwise = ((head input):partial2, rest3)
    where (skipped, rest) =  skipNested input 0 []
          (partial, rest2) = partialInput rest
          (partial2, rest3) = partialInput (tail input)

parseSingle :: [(String, String)] -> [Token] -> [Token] -> ([(Int, Token)], [Token])
parseSingle rules input [r@(NonTerminal _)] = (parseMedicine rules input, [])
parseSingle rules input (r1@(NonTerminal _):r2@(Terminal _):_) = (parseMedicine rules partial, rest)
    where (partial, rest) = traceShow (partialInput input) (partialInput input)

tryRule :: [(String, String)] -> [Token] -> (Token, [Token]) -> [(Int, Token)]
tryRule rules (x:xs) (lhs, r:rs) | withTerminals [r] = if x == r then tryRule rules xs (lhs, rs) else []
                                 | otherwise = trace "Y" traceShow (r:rs) $ deepest $ partial ++ tryRule rules rest (lhs, rs)
    where (partial, rest) = parseSingle rules xs (r:rs)

parseMixed :: [(String, String)] -> [Token] -> [(Int, Token)]
parseMixed rules input = trace "X" $ traceShow input $ nub $ concatMap (tryRule rules input) (traceShow candidateRules candidateRules)
    where candidateRules = filter (withTerminals . snd) tokenizedRules
          tokenizedRules = zip (map (head . tokenize rules . fst) rules) (map (tokenize rules . snd) rules)

shallowest :: [(Int, Token)] -> [(Int, Token)]
shallowest [] = []
shallowest candidates = [minimumBy depth candidates]
    where depth x y = compare (fst x) (fst y)

deepest :: [(Int, Token)] -> [(Int, Token)]
deepest [] = []
deepest candidates = [maximumBy depth candidates]
    where depth x y = compare (fst x) (fst y)

parseMedicine :: [(String, String)] -> [Token] -> [(Int, Token)]
parseMedicine rules input | null right = shallowest $ parsePure rules left
                          | null left = shallowest $ parseMixed rules right
                          | otherwise = traceShow (parseMixed rules right) (parseMixed rules right)
                        --   | otherwise  = traceShow (partialInput (tail input) (Just (Terminal "Y"))) $ parseMixed rules right
                        --   | otherwise = combine (parsePure rules left) (parseMixed rules right)
    where (left, right) = break (==(Terminal "rA")) input

part2 :: String -> String
-- part2 input = show $ parseMedicine (sortBy rhsLength parsedRules) (tokenize parsedRules "e") (tokenize parsedRules medicine)
part2 input = show $ parseMedicine parsedRules (tokenize parsedRules (reverse medicine))
-- part2 input = show $ allReductions parsedRules [] (fst . break (==(Terminal "rA")) $tokenize parsedRules (reverse medicine))
    where (medicine:_:rules) = reverse . lines $ input
          parsedRules = map ((\(lhs, rhs) -> (reverse lhs, reverse rhs)) . either undefined id . parse parseReplacement "") rules
