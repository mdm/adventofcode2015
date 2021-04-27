module Lib
    ( part1
    , part2
    ) where

import Text.Parsec.String (Parser)
import Text.Parsec (string, parse)
import Text.Parsec.Char (digit)
import Text.Parsec.Combinator (many1)
import Data.List (sortBy)
import Debug.Trace(trace)


weapons :: [(Int, Int, Int)]
weapons = [(8, 4, 0), (10, 5, 0), (25, 6, 0), (40, 7, 0), (74, 8, 0)]

armors :: [(Int, Int, Int)]
armors = [(0, 0, 0), (13, 0, 1), (31, 0, 2), (53, 0, 3), (75, 0, 4), (102, 0, 5)]

rings :: [(Int, Int, Int)]
rings = [(0, 0, 0), (25, 1, 0), (50, 2, 0), (100, 3, 0), (20, 0, 1), (40, 0, 2), (80, 0, 3)]

cost :: (Int, Int, Int) -> Int
cost (c, _, _) = c

damage :: (Int, Int, Int) -> Int
damage (_, d, _) = d

armor :: (Int, Int, Int) -> Int
armor (_, _, a) = a

totalCost :: [(Int, Int, Int)] -> Int
totalCost = sum . map cost

totalDamage :: [(Int, Int, Int)] -> Int
totalDamage = sum . map damage

totalArmor :: [(Int, Int, Int)] -> Int
totalArmor = sum . map armor

equipments :: [[(Int, Int, Int)]]
equipments = sortBy (\x y -> compare (totalCost x) (totalCost y)) [[w,a,l,r] | w <- weapons, a <- armors, l <- rings, r <- rings, cost l == 0 || cost l /= cost r]

parseBoss :: Parser (Int, Int, Int)
parseBoss = do
                string "Hit Points: "
                hp <- many1 digit
                string "\nDamage: "
                dmg <- many1 digit
                string "\nArmor: "
                arm <- many1 digit
                return (read hp, read dmg, read arm)

playerTurn (bhp, bdmg, barm) hp equipment | hp <= 0 = True
                                          | otherwise = bossTurn (newHp, bdmg, barm) hp equipment
    where newHp = bhp - max 1 (totalDamage equipment - barm)

bossTurn (bhp, bdmg, barm) hp equipment | bhp <= 0 = False
                                        | otherwise = playerTurn (bhp, bdmg, barm) newHp equipment
    where newHp = hp - max 1 (bdmg - totalArmor equipment)

part1 :: String -> String
part1 input = show . totalCost . head . dropWhile (playerTurn boss 100) $ equipments
    where boss = either undefined id . parse parseBoss "" $ input


part2 :: String -> String
part2 input = show . totalCost . head . dropWhile (not . playerTurn boss 100) $ reverse equipments
    where boss = either undefined id . parse parseBoss "" $ input
