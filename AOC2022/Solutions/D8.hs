module Solutions.D8 where

import Data.List

    {- part 1 -}
isGreaterThanMax :: Int -> [Int] -> [Bool]
isGreaterThanMax _ [] = []
isGreaterThanMax max (num:nums) = case num > max of
                                    True -> True : (isGreaterThanMax num nums)
                                    False-> False: (isGreaterThanMax max nums)

isVisibleFromLeft, isVisibleFromRight, isVisibleFromTop, isVisibleFromBottom :: [[Int]] -> [[Bool]]
isVisibleFromLeft = map (isGreaterThanMax (-1))
isVisibleFromRight = map (reverse . isGreaterThanMax (-1) . reverse)
isVisibleFromTop = transpose . map (isGreaterThanMax (-1)) . transpose
isVisibleFromBottom = transpose . map (reverse . isGreaterThanMax (-1) . reverse) . transpose

isVisibleFromAllSides :: [[Int]] -> [[Bool]]
isVisibleFromAllSides x = foldr1 (zipWith (zipWith (||))) $ map ($ x) [isVisibleFromLeft, isVisibleFromRight, isVisibleFromTop, isVisibleFromBottom]

sumTrue :: Int -> [Bool] -> Int
sumTrue sum [] = sum
sumTrue sum (True:bs) = sumTrue (sum+1) bs
sumTrue sum (False:bs) = sumTrue sum bs

solveD8P1 :: String -> String
solveD8P1 = show . foldr1 (+) . map (sumTrue 0) . isVisibleFromAllSides . map (map (read . pure :: Char -> Int)) . lines

    {- part 2 -}
mapOverLast :: (a -> a) -> [[a]] -> [[a]]
mapOverLast func (last:[])= (map func last):[]
mapOverLast func (a:rest) = a:(mapOverLast func rest)

getViewScoreFromLeft :: Ord a => [a] -> [Int]
getViewScoreFromLeft [] = []
getViewScoreFromLeft (x:y:[]) = [1,0]
getViewScoreFromLeft (x:xs) = (length $ takeWhile (==True) $ [True]++(map (< x) $ init xs)) : getViewScoreFromLeft xs

getViewScore :: [[Int]] -> [Int]
getViewScore x = foldr1 (zipWith (*)) $ map (concat . ($ x)) [ map getViewScoreFromLeft
               , map (reverse . getViewScoreFromLeft . reverse)
               , transpose . map getViewScoreFromLeft . transpose
               , transpose . map (reverse . getViewScoreFromLeft . reverse) . transpose]

solveD8P2 :: String -> String
solveD8P2 = show . foldr1 max . getViewScore . map (map (read . pure :: Char -> Int)) . lines
