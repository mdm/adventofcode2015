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
import Data.Char (isAsciiUpper)


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

allReductions :: [(String, String)] -> [Token] -> [Token] -> [[Token]]
allReductions _ _ [] = []
allReductions _ _ [x] = []
allReductions rules acc ((NonTerminal x1):(NonTerminal x2):tokens) = map (\(lhs, rhs) -> acc ++ NonTerminal lhs:tokens) (matchingRules (x1 ++ x2)) ++ allReductions rules (acc ++ [NonTerminal x1]) (NonTerminal x2:tokens)
    where matchingRules rhs = filter ((==rhs) . snd) rules

parsePure :: [(String, String)] -> [Token] -> [(Int, [Token])]
parsePure rules [x] = [(0, [x])]
parsePure rules input = nub $ concatMap (map plusOne . parsePure rules) (allReductions rules [] input)
    where plusOne (depth, x) = (depth + 1, x)

replace2 :: [Token] -> ([Token], [Token]) -> [[Token]]
replace2 [] _ = [[]]
replace2 start@(x:xs) rule@(input, output) | output `isPrefixOf` start = (input ++ rest):map (x:) (replace2 xs rule)
                                           | otherwise = map (x:) (replace2 xs rule)
    where rest = drop (length output) start

parsePure2 :: [(String, String)] -> [Token] -> [(Int, [Token])]
parsePure2 rules [x] = [(0, [x])]
-- parsePure2 rules input = nub $ concatMap (map plusOne . parsePure2 rules) $ filter (/=input) $ concatMap (\r -> trace (show input ++ " " ++ show r) $ replace2 input r) tokenizedRules
parsePure2 rules input = nub $ concatMap (map plusOne . parsePure2 rules) $ filter (/=input) $ concatMap (replace2 input) tokenizedRules
    where plusOne (depth, x) = (depth + 1, x)
          tokenizedRules = zip (map (tokenize rules "" . fst) rules) (map (tokenize rules "" . snd) rules)

withTerminals :: [Token] -> Bool
withTerminals [] = False
withTerminals ((Terminal x):xs) = True
withTerminals ((NonTerminal x):xs) = withTerminals xs

shallowest :: [(Int, [Token])] -> [(Int, [Token])]
shallowest [] = []
shallowest candidates = [minimumBy depth candidates]
    where depth x y = compare (fst x) (fst y)

deepest :: [(Int, [Token])] -> [(Int, [Token])]
deepest [] = []
deepest candidates = [maximumBy depth candidates]
    where depth x y = compare (fst x) (fst y)

prependToken :: [(String, String)] -> String-> [Token] -> [Token]
prependToken rules acc xs = if null acc then xs else (makeToken $ reverse acc):xs
    where lefts = map fst rules
          makeToken ys | ys `elem` lefts = NonTerminal ys
                       | otherwise = Terminal ys

tokenize :: [(String, String)] -> String -> String -> [Token]
tokenize rules acc [] = prependToken rules acc []
tokenize rules acc (x:xs) | isAsciiUpper x && acc /= "C" = prependToken rules acc $ tokenize rules [x] xs
                          | otherwise = tokenize rules (x:acc) xs

showTokens :: [Token] -> [String]
showTokens [] = []
showTokens ((Terminal t):xs) = t:showTokens xs
showTokens ((NonTerminal t):xs) = t:showTokens xs

prependAcc :: [a] -> [[a]] -> [[a]]
prependAcc acc xs = if null acc then xs else (reverse acc):xs

combineNested :: [Token] -> Int -> [Token] -> ([Token], [Token])
combineNested [] _ acc = (reverse acc, [])
combineNested input count acc | head input == Terminal "Rn" || head input == Terminal "CRn" = combineNested (tail input) (count + 1) ((head input):acc)
                              | head input == Terminal "Ar" && count == 0 = (reverse (head input:acc), tail input)
                              | head input == Terminal "Ar" = combineNested (tail input) (count - 1) ((head input):acc)
                              | otherwise = combineNested (tail input) count ((head input):acc)

splitInput :: Bool -> [Token] -> [Token] -> [[Token]]
splitInput _ acc [] = prependAcc acc []
splitInput skip acc (x@(Terminal t):xs) | (t == "Rn" || t == "CRn") && not skip = prependAcc (x:acc) $ splitInput True [] xs
                                        | t == "Rn" || t == "CRn" = splitInput skip (reverse combined ++ (x:acc)) rest
                                        | t == "Ar" = prependAcc acc $ splitInput False [x] xs
                                        | otherwise = prependAcc acc $ [x]:splitInput skip [] xs
    where (combined, rest) =  combineNested xs 0 []
splitInput skip acc (x@(NonTerminal _):xs) = splitInput skip (x:acc) xs

isTerminal (Terminal _) = True
isTerminal (NonTerminal _) = False

parsePartial :: [(String, String)] -> [Token] -> (Int, [Token])
parsePartial rules input | isTerminal (head input) || isTerminal (last input) = (0, input)
                         | otherwise = parseInput rules input

parseInput :: [(String, String)] -> [Token] -> (Int, [Token])
parseInput rules input | length parts > 1 = (stepOneDepth + stepTwoDepth, stepTwoOutput)
                       | otherwise = head . shallowest $ parsePure2 rules $ head parts
    where parts = splitInput False [] input
          parsedParts = map (parsePartial rules) parts
          stepOneOutput = concatMap snd parsedParts
          stepOneDepth = maximum . map fst $ parsedParts
          (stepTwoDepth, stepTwoOutput) = head . shallowest $ parsePure2 rules stepOneOutput

part2 :: String -> String
-- part2 input = show $ showTokens $ (!!20) $ splitInput False [] $ tokenize parsedRules "" medicine
-- part2 input = show $ map showTokens $ splitInput False [] $ tokenize parsedRules "" medicine
part2 input = show $ parseInput parsedRules $ tokenize parsedRules "" medicine
-- part2 input = show $ parseInput parsedRules $ tokenize parsedRules "" "TiBSiTh"
-- part2 input = show $ parseInput parsedRules $ tokenize parsedRules "" "TiB"
    where (medicine:_:rules) = reverse . lines $ input
          parsedRules = map (either undefined id . parse parseReplacement "") rules
