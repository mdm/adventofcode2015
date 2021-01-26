module Lib
    ( part1
    , part2
    ) where


pairs :: [a] -> [(a, a)]
pairs xs = zip xs (tail xs)

vowels :: String -> Bool
vowels = (>=3) . length . filter (`elem` "aiueo")

doubles :: String -> Bool
doubles = any (uncurry (==)) . pairs

forbidden :: String -> Bool
forbidden xs = any (flip elem $ pairs xs) [('a', 'b'), ('c', 'd'), ('p', 'q'), ('x', 'y')]

pairs2 :: [a] -> [(a, a)]
pairs2 xs = zip xs (tail $ tail xs)

doubles2 :: String -> Bool
doubles2 = any (uncurry (==)) . pairs2

twins :: String -> Bool
twins xs = not $ null [snd a | a <- enumerated, b <- drop (fst a + 2) enumerated, snd a == snd b]
    where enumerated = zip [0..] $ pairs xs

part1 :: String -> String
part1 = show . length . filter nice . lines
    where nice xs = all ($ xs) [vowels, doubles, not . forbidden]

part2 :: String -> String
part2 = show . length . filter nice . lines
    where nice xs = all ($ xs) [twins, doubles2]
