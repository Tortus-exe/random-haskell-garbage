module Solutions.D7 where

-- parents and size
data File = File String [String] Int | Dir String [String] Int deriving Show

getParent :: File -> [String]
getParent (File _ p s) = p
getParent (Dir _ p s) = p

sizeOf :: File -> Int
sizeOf (File _ p s) = s
sizeOf (Dir _ p s) = s

getName :: File -> String
getName (File n _ _) = n
getName (Dir n _ _) = n

isDir :: File -> Bool
isDir (File _ _ _) = False
isDir (Dir _ _ _) = True

    {- part 1 -}
constructTree :: [String] -> [File] -> [String] -> [File]
constructTree [] f [] = f
constructTree (p:ps) f [] = constructTree ps ([Dir p ps $ foldr1(+) $ map sizeOf $ filter ((==(p:ps)) . getParent) f] ++ f) []
constructTree parents files (cmd:cmds) = 
    case words cmd of
        ("$":"cd":"..":_) -> constructTree (tail parents) ([Dir (head parents) (tail parents) (
            foldr1(+) $ map sizeOf $ filter ((==parents) . getParent) files)] ++ files) cmds
        ("$":"ls":_) -> constructTree parents files cmds
        ("$":"cd":dir:_) -> constructTree ([dir]++parents) files cmds
        ("dir":name:_) -> constructTree parents files cmds
        (size:name:_) -> constructTree parents ([File name parents $ (read::String -> Int) size]++files) cmds

solveD7P1 :: String -> String
solveD7P1 = show . foldr1 (+) . map sizeOf . filter (isDir) . filter ((<100_000) . sizeOf) . constructTree [] [] . lines

    {- part 2 -}
totalSpace = 70_000_000
spaceNeeded = 30_000_000

remainingSpace :: [File] -> Int
remainingSpace = head . map sizeOf . filter ((=="/") . getName)

getAllFilesLargerThan :: ([File] -> Int) -> [File] -> [File]
getAllFilesLargerThan discriminant files = let size = discriminant files
                                           in filter ((> size) . sizeOf) files

solveD7P2 :: String -> String
solveD7P2 = show . foldr1 min . map sizeOf . getAllFilesLargerThan ((spaceNeeded -) . (totalSpace -) . remainingSpace) . filter (isDir) . constructTree [] [] . lines
