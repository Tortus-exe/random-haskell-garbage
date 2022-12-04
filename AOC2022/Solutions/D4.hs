module Solutions.D4 where

import Data.List
import Data.List.Split
import Control.Monad

    {- part 1 -}
oneRangeInsideOther :: [[Integer]] -> Bool
oneRangeInsideOther x = let a = head $ head x
                            b = (head x) !! 1
                            c = head (x !! 1)
                            d = (x !! 1) !! 1
                          in not $ ((c>a) && (d>b)) || ((c<a) && (d<b))

solveD4P1 :: String -> String
solveD4P1 = show . length . filter (oneRangeInsideOther) . map (map (map read . wordsBy (=='-')) . wordsBy (==',')) . lines

    {- part 2 -}
overlappingRange :: [[Integer]] -> Bool
overlappingRange x = let a = head $ head x
                         b = (head x) !! 1
                         c = head (x !! 1)
                         d = (x !! 1) !! 1
                     in not $ (d < a) || (b < c)

solveD4P2 :: String -> String
solveD4P2 = show . length . filter (overlappingRange) . map (map (map read . wordsBy (=='-')) . wordsBy (==',')) . lines
