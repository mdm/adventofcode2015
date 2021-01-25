module Lib
    ( part1
    , part2
    ) where

import Data.Char(ord)
import Crypto.Hash(hashWith, MD5(MD5))
import Data.ByteString(ByteString, pack, unpack)

startsWithZeroes :: Int -> [Char] -> Bool
startsWithZeroes n = and . zipWith (==) (replicate n '0')

hash :: String -> Int -> String
hash input n = show . hashWith MD5 . pack . map (fromIntegral . ord) $ (init input ++ show n)

part1 :: String -> String
part1 input = show $ until (startsWithZeroes 5 . hash input) (+1) 0

part2 :: String -> String
part2 input = show $ until (startsWithZeroes 6 . hash input) (+1) 0

