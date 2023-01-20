module Solutions.D11 where

import Data.List.Split
import Data.List
import Data.Function

--newtype Monkey = Monkey ([Int], Int -> Int, Int -> Int)
-- use a record instead stupid

data Monkey = Monkey { items :: [Int]
					 , operation :: Int -> Int
					 , test :: Int -> Int
					 }

    {- part 1 -}
put :: Int -> b -> [b] -> [b]
put x element orig = (take x orig)++[element]++(drop (x+1) orig)

parseList :: String -> [Int]
parseList = read . (++"]") . ("["++) . drop 16

parseOp :: String -> Int -> Int
parseOp x old = case drop 17 x of
              "old * old" -> old * old `div` 3
              _ -> case drop 21 x of
                     ('*':' ':xs) -> old * (read xs :: Int) `div` 3
                     ('+':' ':xs) -> (old + (read xs :: Int)) `div` 3
parseTest :: [String] -> Int -> Int
parseTest testText old = if old `mod` ((read $ drop 19 $ head testText) :: Int) == 0 
                            then (read $ drop 25 $ (testText !! 1)) :: Int
                            else (read $ drop 25 $ (testText !! 2)) :: Int

parseMonkey :: [String] -> Monkey
parseMonkey x = Monkey { items = parseList $ head filtered 
                       , operation = parseOp $ filtered !! 1
                       , test = parseTest $ drop 2 filtered
                       }
                    where filtered = map (dropWhile (==' ')) $ tail x 

evolveMonkey :: Int -> [Monkey] -> [Monkey]
evolveMonkey 0 monkeys = monkeys
evolveMonkey times monkeys = evolveMonkey (times-1) evolveMonkey' (length monkeys) monkeys

evolveMonkey' :: Int -> [Monkey] -> [Monkey]
evolveMonkey' 0 monkeys = monkeys
evolveMonkey' counter monkeys = undefined --ergh head hurty

solveD11P1 :: String -> String
solveD11P1 = show . map items . map parseMonkey . splitWhen (=="") . lines
--show . evolveMonkey 1 0 8 . map parseMonkey . splitWhen (=="") . lines

    {- part 2 -}
solveD11P2 :: String -> String
solveD11P2 = show . lines
