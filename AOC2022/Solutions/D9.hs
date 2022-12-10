module Solutions.D9 where

import Data.List

    {- part 1 -}
type HeadPos = (Int, Int)
type TailPos = (Int, Int)

data Move = U Int 
          | R Int 
          | D Int 
          | L Int
          deriving (Eq, Read, Show)

newtype State = State (HeadPos, TailPos, [TailPos]) deriving (Eq, Show)

newtype State2 = State2 ([HeadPos], [TailPos]) deriving (Eq, Show)

moveHeadAndTail :: Move -> State -> State
moveHeadAndTail (U 0) state = state
moveHeadAndTail (D 0) state = state
moveHeadAndTail (R 0) state = state
moveHeadAndTail (L 0) state = state
moveHeadAndTail (U num) (State ((hx, hy), (tx, ty), movelist)) = 
    let newHeadPos = (hx, hy+1)
        newTailPos = if (hy+1-ty >= 2) then (hx, hy) else (tx, ty)
    in moveHeadAndTail (U (num-1)) $ State (newHeadPos, newTailPos, newTailPos : movelist)
moveHeadAndTail (R num) (State ((hx, hy), (tx, ty), movelist)) = 
    let newHeadPos = (hx+1, hy)
        newTailPos = if (hx+1-tx >= 2) then (hx, hy) else (tx, ty)
    in moveHeadAndTail (R (num-1)) $ State (newHeadPos, newTailPos, newTailPos : movelist)
moveHeadAndTail (D num) (State ((hx, hy), (tx, ty), movelist)) = 
    let newHeadPos = (hx, hy-1)
        newTailPos = if (hy-1-ty <= (-2)) then (hx, hy) else (tx, ty)
    in moveHeadAndTail (D (num-1)) $ State (newHeadPos, newTailPos, newTailPos : movelist)
moveHeadAndTail (L num) (State ((hx, hy), (tx, ty), movelist)) = 
    let newHeadPos = (hx-1, hy)
        newTailPos = if (hx-1-tx <= (-2)) then (hx, hy) else (tx, ty)
    in moveHeadAndTail (L (num-1)) $ State (newHeadPos, newTailPos, newTailPos : movelist)

moveRope :: [Move] -> State -> State
moveRope [] state = state
moveRope (move:moves) state = moveRope moves (moveHeadAndTail move state)

solveD9P1 :: String -> String
solveD9P1 = show . length . nub . (\(State(_,_,hist))->hist) . flip moveRope (State ((0,0), (0,0), [])) . map (read :: String -> Move) . lines

    {- part 2 -}
newPos' :: HeadPos -> [HeadPos] -> [HeadPos]
newPos' _ (final:[]) = []
newPos' (newx,newy) ((hx,hy):(tx,ty):rest) = 
    case (2<=(abs $ tx-newx), 2<=(abs $ ty-newy)) of
        (False, False) -> [(tx,ty)]++(newPos' (tx,ty) ((tx,ty):rest))
        _ -> let newpos = ((tx+(signum $ newx - tx)), ty+(signum $ newy - ty))
             in [newpos]++(newPos' newpos ((tx,ty):rest))

newPos :: Move -> State2 -> State2
newPos (U 0) cache = cache
newPos (R 0) cache = cache
newPos (D 0) cache = cache
newPos (L 0) cache = cache
newPos (U num) (State2 (((hx,hy):restpos), tailCache)) = let new = newPos' (hx,hy+1) ((hx,hy):restpos)
                                                             prevTail = last restpos
                                                         in newPos (U $ num-1) $ State2 (((hx,hy+1):new), (prevTail:tailCache))
newPos (R num) (State2 (((hx,hy):restpos), tailCache)) = let new = newPos' (hx+1,hy) ((hx,hy):restpos)
                                                             prevTail = last restpos
                                                         in newPos (R $ num-1) $ State2 (((hx+1,hy):new), (prevTail:tailCache))
newPos (D num) (State2 (((hx,hy):restpos), tailCache)) = let new = newPos' (hx,hy-1) ((hx,hy):restpos)
                                                             prevTail = last restpos
                                                         in newPos (D $ num-1) $ State2 (((hx,hy-1):new), (prevTail:tailCache))
newPos (L num) (State2 (((hx,hy):restpos), tailCache)) = let new = newPos' (hx-1,hy) ((hx,hy):restpos)
                                                             prevTail = last restpos
                                                         in newPos (L $ num-1) $ State2 (((hx-1,hy):new), (prevTail:tailCache))

doMovesBig :: [Move] -> State2 -> State2
doMovesBig [] state = state
doMovesBig (move:moves) state = doMovesBig moves $ newPos move state

solveD9P2 :: String -> String
solveD9P2 = show . length . nub . (\(State2 (x,cache))->([last x])++cache) . flip doMovesBig initialState . map (read :: String -> Move) . lines

initialState :: State2
initialState = State2 ([(0,0),(0,0),(0,0),(0,0),(0,0),(0,0),(0,0),(0,0),(0,0),(0,0)], [])
