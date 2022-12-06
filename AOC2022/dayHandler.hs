module Main where

import Solutions.D1
import Solutions.D2
import Solutions.D3
import Solutions.D4
import Solutions.D5
import Solutions.D6

import System.IO
import System.Environment

errorMessage = unlines $ ["invalid input! Please input formatted:", 
                          "./d1 [day] [part] [file]",
                          "where [day] is from 1 to 25 [part] is either 1 or 2, and [file] is the input for that part."]

solutionsPart1Table = [
            solveD1P1,
            solveD2P1,
            solveD3P1,
            solveD4P1,
            solveD5P1,
            solveD6P1
                      ]

solutionsPart2Table = [
            solveD1P2,
            solveD2P2,
            solveD3P2,
            solveD4P2,
            solveD5P2,
            solveD6P2
                      ]

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
