module Main where

import Solutions.D1
import Solutions.D2

import System.IO
import System.Environment

errorMessage = unlines $ ["invalid input! Please input formatted:", 
                          "./d1 [day] [part] [file]",
                          "where [day] is from 1 to 25 [part] is either 1 or 2, and [file] is the input for that part."]

solutionsPart1Table = [
            solveD1P1,
            solveD2P1]

solutionsPart2Table = [
            solveD1P2,
            solveD2P2]

main :: IO ()
main = do
    args <- getArgs
    if length args < 2
        then putStrLn errorMessage
        else do
            let day = (read . head) args
                part = read $ args !! 1
                filename = args !! 2
            handle <- openFile (filename) ReadMode
            problemInput <- hGetContents handle
            let output = case part of
                            1 -> (solutionsPart1Table !! (day-1)) problemInput
                            2 -> (solutionsPart2Table !! (day-1)) problemInput
                            _ -> errorMessage
            putStrLn output
            hClose handle
