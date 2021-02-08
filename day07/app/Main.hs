module Main where

import System.Environment
import Lib (part1, part2)

main :: IO ()
main = do
    args <- getArgs
    input <- readFile (head args)
    putStrLn (part1 input)
    putStrLn (part2 input)
