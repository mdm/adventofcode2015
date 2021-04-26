module Lib
    ( part1
    , part2
    ) where

import Debug.Trace(trace)
import Data.List (group)
import Control.Arrow ((&&&))


isPrime :: Int -> Bool
primes :: [Int]

isPrime n | n < 2 = False
isPrime n = all (\p -> n `mod` p /= 0) . takeWhile ((<= n) . (^ 2)) $ primes
primes = 2 : filter isPrime [3..]

primeFactors' :: Int -> [Int]
primeFactors' n = iter n primes where
    iter n (p:_) | n < p^2 = [n | n > 1]
    iter n ps@(p:ps') =
        let (d, r) = n `divMod` p
        in if r == 0 then p : iter d ps else iter n ps'

primeFactors :: Int -> [(Int, Int)]
primeFactors n = (map (head &&& length) . group) (primeFactors' n)

minHouseNumber1 :: Int -> Int -> Int
minHouseNumber1 n t | presents < t = minHouseNumber1 (n + 1) t
                   | otherwise = n
    where presents = product $ map (\(p, e) -> sum [p^i | i <- [0..e]]) $ primeFactors n

part1 :: String -> String
part1 = show . minHouseNumber1 2 . (`div` 10) . read . head . lines

combine xs ys = [x * y | x <- xs, y <- ys]

minHouseNumber2 :: Int -> Int -> Int
minHouseNumber2 n t | 11 * presents < t = minHouseNumber2 (n + 1) t
                    | otherwise = n
    where presents = sum $ filter (\x -> n `div` x < 50) $ foldr1 combine $ map (\(p, e) -> [p^i | i <- [0..e]]) $ primeFactors n

part2 :: String -> String
part2 = show . minHouseNumber2 2 . read . head . lines

