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

-- rhsLength:: (String, String) -> (String, String) -> Ordering 
-- rhsLength (_, a) (_, b) = compare (length b) (length a)

-- choice :: [(String, String)] -> [Token] -> [Token] -> Maybe Int
-- choice rules ((NonTerminal x):xs) ys | length xs < length ys = if null options then Nothing else Just $ 1 + head options
--                                      | otherwise = Nothing
--     where matchingRules = filter ((==x) . fst) rules
--           applyRule (_, zs) = parseMedicine rules (tokenize rules zs ++ xs) ys
--           options = mapMaybe applyRule matchingRules

-- parseMedicine :: [(String, String)] -> [Token] -> [Token] -> Maybe Int
-- parseMedicine rules [] ys = if null ys then Just 0 else Nothing
-- parseMedicine rules _ [] = Nothing
-- parseMedicine rules ((Terminal x):xs) ((Terminal y):ys) = if x == y then parseMedicine rules xs ys else Nothing
-- parseMedicine rules ((Terminal x):xs) ((NonTerminal y):ys) = Nothing
-- parseMedicine rules start@((NonTerminal x):xs) target@((Terminal y):ys) = choice rules start target
-- parseMedicine rules start@((NonTerminal x):xs) target@((NonTerminal y):ys) = msum [if x == y then parseMedicine rules xs ys else Nothing, choice rules start target]

-- withoutTerminals [] = True
-- withoutTerminals ((Terminal x):xs) = False
-- withoutTerminals ((NonTerminal x):xs) = withoutTerminals xs

-- parseRight rules x ys = map applyRule matchingRules
--     where matchingRules = filter (withoutTerminals . tokenize rules . snd) . filter ( . (==x) . fst) rules
--           applyRule (lhs, _:rhs:[]) = (lhs, parseRight' rules rhs ys)

-- choice' :: [(String, String)] -> String -> [Token] -> [Token] Maybe Int
-- choice' rules x left right = 
--     where matchingRules = filter ((==x) . fst) rules

-- parseQuickly :: [(String, String)] -> String -> [Token] Maybe Int
-- parseQuickly rules x ys = maximum' $ choice' (left ++ [ar]) right
--     where (left, ar:right) = break (==(Terminal "Ar")) ys

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

nextTerminal :: [Token] -> Maybe Token
nextTerminal [] = Nothing
nextTerminal ((Terminal x):xs) = Just $ Terminal x
nextTerminal ((NonTerminal x):xs) = nextTerminal xs

skipNested :: [Token] -> Int -> [Token] -> ([Token], [Token])
skipNested [] _ acc = (reverse acc, [])
skipNested input count acc | head input == Terminal "rA" = skipNested (tail input) (count + 1) ((head input):acc)
                           | head input == Terminal "nR" = if count == 0 then (reverse acc, tail input) else skipNested (tail input) (count - 1) ((head input):acc)
                           | otherwise = skipNested (tail input) count ((head input):acc)

partialInput :: [Token] -> Maybe Token -> [Token]
partialInput [] _ = []
partialInput input Nothing = input
partialInput input (Just end) | head input == end = []
                              | head input == Terminal "rA" = skipped ++ partialInput rest (Just end)
                              | otherwise = (head input):(partialInput (tail input) (Just end))
    where (skipped, rest) =  undefined


tryRule :: [(String, String)] -> [Token] -> (Token, [Token]) -> [(Int, Token)]
tryRule rules (x:xs) (lhs, r:rs) | withTerminals [r] = tryRule rules xs (lhs, rs)
                                 | otherwise = undefined

parseMixed :: [(String, String)] -> [Token] -> [(Int, Token)]
parseMixed rules input = nub  $ concatMap (tryRule rules input) candidateRules
    where candidateRules = filter (withTerminals . snd) tokenizedRules
          tokenizedRules = zip (map (head . tokenize rules . fst) rules) (map (tokenize rules . snd) rules)

parseMedicine :: [(String, String)] -> [Token] -> [(Int, Token)]
parseMedicine rules input | null right = parsePure rules left
                          | otherwise  = parseMixed rules right
                        --   | otherwise = combine (parsePure rules left) (parseMixed rules right)
    where (left, right) = break (==(Terminal "rA")) input

part2 :: String -> String
-- part2 input = show $ parseMedicine (sortBy rhsLength parsedRules) (tokenize parsedRules "e") (tokenize parsedRules medicine)
part2 input = show $ parseMedicine parsedRules (tokenize parsedRules (reverse medicine))
-- part2 input = show $ allReductions parsedRules [] (fst . break (==(Terminal "rA")) $tokenize parsedRules (reverse medicine))
    where (medicine:_:rules) = reverse . lines $ input
          parsedRules = map ((\(lhs, rhs) -> (reverse lhs, reverse rhs)) . either undefined id . parse parseReplacement "") rules
