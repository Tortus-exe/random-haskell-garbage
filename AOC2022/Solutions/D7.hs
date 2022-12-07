module Solutions.D7 where

newtype File = File String Int

    {- part 1 -}
constructTree :: [String] -> [String] -> [File]
constructTree parents (cmd:cmds) = 
    case cmd of
        "$ cd .." -> let filetree = constructTree (drop 1 parents) cmds
                     in map sumSizeOf filetree

solveD7P1 :: String -> String
solveD7P1 = show . constructTree [] . lines

    {- part 2 -}
solveD7P2 :: String -> String
solveD7P2 = show . lines
