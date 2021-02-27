module Lib
    ( part1
    , part2
    ) where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (mapMaybe)

type Grid = Map (Int, Int) Char

enumerate :: [a] -> [(Int, a)]
enumerate = zip [0..]

insertTile :: Int -> (Int, Char) -> Grid -> Grid
insertTile y (x, tile) = Map.insert (y, x) tile

insertRow :: (Int, [(Int, Char)]) -> Grid
insertRow (y, xs) = foldr (insertTile y) Map.empty xs

neighbors :: Grid -> (Int, Int) -> [Char]
neighbors grid (y, x) = mapMaybe (grid Map.!?) coords
    where coords = [(y - 1, x - 1), (y - 1, x), (y - 1, x + 1), (y, x - 1), (y, x + 1), (y + 1, x - 1), (y + 1, x), (y + 1, x + 1)]

update :: Grid -> (Int, Int) -> Char -> Char
update grid coords tile | tile == '#' = if on == 2 || on == 3 then '#' else '.'
                        | tile == '.' = if on == 3 then '#' else '.'
                        | otherwise = tile
    where on = length . filter (/='.') $ neighbors grid coords

step :: Grid -> Grid
step grid = Map.mapWithKey (update grid) grid

part1 :: String -> String
part1 = show . length . filter (/='.') . Map.elems . (!! 100) . iterate step . Map.unions . map insertRow . enumerate . map enumerate . lines

part2 :: String -> String
part2 = const ""
