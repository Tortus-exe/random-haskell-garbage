module Solutions.D10 where

import Data.List.Split

    {- part 1 -}
parse :: [[String]] -> [Int]
parse [] = []
parse (instruction:rest) | head instruction == "addx" = [0,(read :: String -> Int) $ last instruction]++(parse rest)
                         | otherwise = 0:(parse rest)

solveD10P1 :: String -> String
solveD10P1 = show . foldr1 (+) . zipWith (*) [20,60..] . scanl1 (+) . map (foldr1 (+)) . chunksOf 40 . take 240 . (([1]++[0|k<-[1..20]])++) . parse . map words . lines

    {- part 2 -}
solveD10P2 :: String -> String
solveD10P2 = unlines . map (map (\x ->if 1>=abs x then '█' else ' ') . zipWith (-) [0..40]) . chunksOf 40 . init . scanl (+) 1 . take 240 . parse . map words . lines
