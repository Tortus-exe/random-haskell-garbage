module Solutions.D6 where

import Data.List
import Control.Monad

    {- part 1 -}
takeUntilNoRepeats' :: Int -> Int -> String -> Int
takeUntilNoRepeats' num count list = if ((==) <*> nub) $ take num list
                                        then count 
                                        else takeUntilNoRepeats' num (count+1) $ tail list

takeUntilNoRepeats :: Int -> String -> Int
takeUntilNoRepeats x = takeUntilNoRepeats' x x

solveD6P1 :: String -> String
solveD6P1 = show . takeUntilNoRepeats 4

    {- part 2 -}
solveD6P2 :: String -> String
solveD6P2 = show . takeUntilNoRepeats 14
