module Main where

import Data.List ( find, groupBy, findIndex )
import Data.List.Split (splitOn)
import Maybes (fromJust)
import Data.Maybe (fromMaybe)
import GHC.IO (evaluate)

main :: IO ()
main = do
    input <- readFile "input.txt"
    let parsed = justParse input
    evaluate parsed
    print "parsing completed"
    printRoot parsed
    print $ partTwo parsed

printRoot :: DirItem -> IO ()
printRoot dir = do mapM_ putStrLn $ printDir dir

--magic numbers
partOneMaxSize :: Int
partOneMaxSize = 100000
partTwoTotal :: Int
partTwoTotal = 70000000
partTwoMin :: Int
partTwoMin = 30000000

partOne :: DirItem -> String
partOne = show . sum . getSizes (\x-> dirItemSize x <= partOneMaxSize) []

partTwo :: DirItem -> String
partTwo = show . minimum . (\x-> filter (>= minSize x ) $ getSizes (const True) [] x)

minSize :: DirItem -> Int
minSize root = partTwoMin - (partTwoTotal - dirItemSize root)

getSizes :: (DirItem -> Bool) -> [Int] -> DirItem -> [Int]
getSizes _ xs f@File {} = xs
getSizes _ xs (Dir _ []) = xs
getSizes pred xs d | pred d = dirItemSize d : rest ++ xs
                   | otherwise =  rest ++ xs
    where
        rest = concatMap (getSizes pred []) (contents d)

filterSizes :: (DirItem -> Bool) -> DirItem -> DirItem --output is filtered root
filterSizes _ f@File{} = f
filterSizes pred dir   = dir{contents = map (filterSizes pred) $ filter pred (contents dir)}

isNotFile :: DirItem -> Bool
isNotFile f@File {} = False
isNotFile _ = True

justParse :: String -> DirItem
justParse str = runAllCommands ((tail . parseInput) str) makeRoot []

runAllCommands :: [Command] -> DirItem -> DirItemIndex -> DirItem
runAllCommands [] root _          = root
runAllCommands (x:xs) root index  = uncurry (runAllCommands xs) $ runCommand x root index

runCommand :: Command ->  DirItem -> DirItemIndex -> (DirItem, DirItemIndex)
runCommand c@(LS xs) root index  = (addContent index root $ map parseDirItem xs, index)
runCommand c@(CD dirName) root index = (root ,cdCommand root dirName index)

makeRoot :: DirItem
makeRoot = Dir "/" []

getContentFromDirIndex :: DirItemIndex -> DirItem -> [DirItem]
getContentFromDirIndex [] dir = contents dir
getContentFromDirIndex (x:xs) dir = getContentFromDirIndex xs (contents dir!!x)

parentDir :: DirItemIndex -> DirItemIndex
parentDir [] = error "root has no parent"
parentDir xs = init xs

dirItemSize :: DirItem -> Int
dirItemSize f@File {} = size f
dirItemSize (Dir _ []) = 0
dirItemSize dir = (sum . map dirItemSize) $ contents dir

dirSize :: DirItem -> Int
dirSize f@File {} = 0
dirSize (Dir _ []) = 0
dirSize dir = dirItemSize dir + (sum . map dirSize) (contents dir)

allFiles :: [DirItem] -> Bool
allFiles = all isNotFile

cdCommand :: DirItem -> String -> DirItemIndex -> DirItemIndex
cdCommand _ "/" _ = []
cdCommand _ ".." index = parentDir index
cdCommand root str index = index ++ [findDirIndex 0 str (getContentFromDirIndex index root)]-- oei buggs?

findDirIndex :: Int -> String -> [DirItem] -> Int
findDirIndex i str [] = error ("no '" ++ str ++ "' in '" ++ "directory" ++ "', ended at: " ++ show i)
findDirIndex i str (x:xs) | name x == str = i
                            | otherwise = findDirIndex (i+1) str xs

addContent :: DirItemIndex -> DirItem -> [DirItem] -> DirItem
addContent _ f@File{} new = error "no contents in files"
addContent index dir new  = addAtIndex index dir new

addAtIndex :: DirItemIndex -> DirItem -> [DirItem] -> DirItem
addAtIndex [] dir content     = dir{contents = content}
addAtIndex (x:xs) dir content = dir{contents = replace (addContent xs (current!!x) content) x current}
    where
        current = contents dir

replace :: a -> Int -> [a] -> [a]
replace x _ [] = [x]
replace x i xs = left ++ x: tail ys
    where
        (left,ys) = splitAt i xs

parseInput :: String -> [Command]
parseInput = map parseCommand . groupBy (\_ y -> '$' `notElem` y) . lines

parseCommand :: [String] -> Command
parseCommand str | head str == "$ ls" = LS $ tail str
                 | otherwise = CD $ drop 5 $ head str

parseDirItem :: String -> DirItem
parseDirItem str | take 3 str == "dir" = Dir (drop 4 str) []
                 | otherwise = File (last split) (read $ head split)
    where
        split = splitOn " " str

printDir :: DirItem -> [String]
printDir f@File {} = ["- " ++ name f ++ " (size : " ++ (show . size) f ++ ")"]
printDir d@Dir {} = ("- " ++ name d ++ "  \\") : (printContents . contents) d

printContents :: [DirItem] -> [String]
printContents = concatMap (map ("  " ++). printDir)

data Command = LS [String]
            | CD String
    deriving (Show)


data DirItem = File {name :: String, size :: Int }
            | Dir {name :: String, contents :: [DirItem] }
    deriving (Show)

type DirItemIndex = [Int]