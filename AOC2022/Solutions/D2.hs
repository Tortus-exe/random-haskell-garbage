module Solutions.D2 where

import System.Environment
import System.IO
import Data.List
import Control.Monad

pointsForShape = "_XYZ"

        {- part 1 -}
getPoints :: [String] -> Int
getPoints = foldr1 (+) . map (liftM2 (+) getPointsForShape getPointsForWin) 
            where 
                getPointsForShape = head . flip elemIndices pointsForShape . head . drop 1
                getPointsForWin "AY" = 6 
                getPointsForWin "BZ" = 6
                getPointsForWin "CX" = 6
                getPointsForWin "AX" = 3
                getPointsForWin "BY" = 3
                getPointsForWin "CZ" = 3
                getPointsForWin _ = 0

solveD2P1 :: String -> String
solveD2P1 = show . getPoints . map (map head . words) . lines

        {- part 2 -}
pointsForWin = "X__Y__Z"

getPointsForPart2 :: [String] -> Int
getPointsForPart2 = foldr1 (+) . map (liftM2 (+) getPointsForShape getPointsForWin)
            where 
                getPointsForWin = head . flip elemIndices pointsForWin . head . drop 1
                getPointsForShape "AY" = 1
                getPointsForShape "BX" = 1
                getPointsForShape "CZ" = 1
                getPointsForShape "BY" = 2
                getPointsForShape "CX" = 2
                getPointsForShape "AZ" = 2
                getPointsForShape _ = 3

solveD2P2 :: String -> String
solveD2P2 = show . getPointsForPart2 . map (map head . words) . lines
