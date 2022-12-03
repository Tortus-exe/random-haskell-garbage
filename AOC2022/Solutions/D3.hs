module Solutions.D3 where

import Data.List
import Data.List.Split

    {- part 1 -}
priorities = "_abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"

splitToCompartments :: [a] -> ([a], [a])
splitToCompartments x = splitAt (length x `div` 2) x

firstElemSecond :: Eq a => ([a], [a]) -> a
firstElemSecond (first, second) = head $ filter (`elem` second) first

getPriority :: Char -> Int
getPriority char = let ind = elemIndex char priorities in
                       case ind of
                         Just x -> x
                         Nothing -> 0

solveD3P1 :: String -> String
solveD3P1 = (show . foldr1 (+) . map (getPriority . firstElemSecond . splitToCompartments) . lines)

    {- part 2 -}
commonLetterAmongThree :: Eq a => [[a]] -> a
commonLetterAmongThree (x:y:z:[]) = head $ filter (`elem` y) $ filter (`elem` z) x

solveD3P2 :: String -> String
solveD3P2 = (show . foldr1 (+) . map (getPriority . commonLetterAmongThree) . chunksOf 3 . lines)
