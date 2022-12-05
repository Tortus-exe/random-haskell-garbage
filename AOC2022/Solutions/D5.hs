module Solutions.D5 where

import Data.List.Split
import Data.List

padding = [""," ","  ","   "]

    {- part 1 -}
unformat :: [String] -> [[Char]]
unformat stack = let numChunks = (length $ head stack) `div` 4 + 1
                     pad = padding !! ((4 * numChunks) - (length . head) stack)
                 in map (dropWhile (==' ')) 
                  $ transpose 
                  $ take ((length stack) - 1) 
                  $ map (take numChunks . map (!!1) . chunksOf 4 . (++pad)) stack

secondElements :: [a] -> [a]
secondElements [] = []
secondElements (x:y:xs) = [y]++secondElements xs

makeFunctionsFrom :: ([Int] -> [String] -> [String]) -> [String] -> [[String] -> [String]]
makeFunctionsFrom k = map (k . map (read :: String -> Int) . secondElements . words)

transform :: [Int] -> [String] -> [String]
transform (num:from:to:_) array = let taken = reverse $ take num (array !! (from-1))
                                      popped = (take (from-1) array)
                                             ++[drop num (array!!(from-1))]
                                             ++(drop from array)
                                  in (take (to-1) popped)
                                  ++ [taken++(popped!!(to-1))]
                                  ++ (drop to popped)

processTransformations :: ([Int] -> [String] -> [String]) -> [[String]] -> [String]
processTransformations transformer (initState:tfs) = 
            let initial = unformat initState
                transformations = makeFunctionsFrom transformer $ head tfs
             in foldr ($) initial $ reverse transformations

solveD5P1 :: String -> String
solveD5P1 = show . map head . processTransformations transform . splitWhen (=="") . lines

    {- part 2 -}
-- don't reverse the taken elements.
transformP2 :: [Int] -> [String] -> [String]
transformP2 (num:from:to:_) array = let taken = take num (array !! (from-1))
                                        popped = (take (from-1) array)
                                               ++[drop num (array!!(from-1))]
                                               ++(drop from array)
                                    in (take (to-1) popped)
                                    ++ [taken++(popped!!(to-1))]
                                    ++ (drop to popped)

solveD5P2 :: String -> String
solveD5P2 = show . map head . processTransformations transformP2 . splitWhen (=="") . lines
