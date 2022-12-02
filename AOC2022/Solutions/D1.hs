module Solutions.D1 where

import System.Environment
import System.IO
import Data.List

    {- part 1 -}
splitWhen :: (String -> Bool) -> [String] -> [[String]]
splitWhen _ [] = []
splitWhen predicate list = [takeWhile (not . predicate) list] ++ 
                            (splitWhen predicate $
                                drop 1 $
                                dropWhile (not . predicate) list)

sumOfStrings :: [String] -> Int
sumOfStrings = foldr1 (+) . map read

solveD1P1 :: String -> String
solveD1P1 = show . foldr1 max . map (sumOfStrings) . splitWhen (=="") . lines

    {- part 2 -}
solveD1P2 :: String -> String
solveD1P2 = show . foldr1 (+) . getTopThree. map (sumOfStrings) . splitWhen (=="") . lines
        where
            getTopThree = take 3 . reverse . sort
