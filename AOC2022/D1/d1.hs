module Main where

import System.Environment
import System.IO
import Data.List

errorMessage = unlines $ ["invalid input! Please input formatted:", 
                          "./d1 [part] [file]",
                          "where [part] is either 1 or 2, and [file] is the input for that part."]

    {- part 1 -}
splitWhen :: (String -> Bool) -> [String] -> [[String]]
splitWhen _ [] = []
splitWhen predicate list = [takeWhile (not . predicate) list] ++ 
                            (splitWhen predicate $
                                drop 1 $
                                dropWhile (not . predicate) list)

sumOfStrings :: [String] -> Int
sumOfStrings = foldr1 (+) . map read

solvePart1 :: String -> String
solvePart1 = show . foldr1 max . map (sumOfStrings) . splitWhen (=="") . lines

    {- part 2 -}
solvePart2 :: String -> String
solvePart2 = show . foldr1 (+) . getTopThree. map (sumOfStrings) . splitWhen (=="") . lines
        where
            getTopThree = take 3 . reverse . sort


main :: IO ()
main = do
    args <- getArgs
    if length args < 2
        then putStrLn errorMessage
        else do
            let part = (read . head) args
                filename = args !! 1
            handle <- openFile (filename) ReadMode
            problemInput <- hGetContents handle
            let output = case part of
                            1 -> solvePart1 problemInput
                            2 -> solvePart2 problemInput
                            _ -> errorMessage
            putStrLn output
            hClose handle
