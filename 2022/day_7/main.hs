module Main where

import Data.List ( find, groupBy, findIndex )
import Data.List.Split (splitOn)
import Maybes (fromJust)
import Data.Maybe (fromMaybe)
import GHC.IO (evaluate)

main :: IO ()
main = do
    input <- readFile "example.txt"
    let parsed = justParse input
    evaluate parsed
    print "parsing completed"
    mapM_ putStrLn $ printDir parsed
    print $ partOne parsed

partOneMaxSize :: Int
partOneMaxSize = 100000

partOne :: DirItem -> String
partOne = show . dirSize . filterSizes

filterSizes :: DirItem -> DirItem --output is filtered root
filterSizes f@File{} = f
filterSizes dir      = dir{contents = map filterSizes $ filter (\x-> dirItemSize x <= partOneMaxSize)(contents dir)}

isNotFile :: DirItem -> Bool
isNotFile f@File {} = False
isNotFile _ = True

partPrint :: String -> [String]
partPrint = printDir . justParse

justParse :: String -> DirItem
justParse str = runAllCommands ((tail . parseInput) str) makeRoot []

runAllCommands :: [Command] -> DirItem -> DirItemIndex -> DirItem
runAllCommands [] root _          = root
runAllCommands (x:xs) root index  = uncurry (runAllCommands xs) $ runCommand x root index

runCommand :: Command ->  DirItem -> DirItemIndex -> (DirItem, DirItemIndex)
runCommand c@(LS xs) root index  = (addContent index root $ map parseDirItem xs, index)
runCommand c@(CD dir) root current = (root ,cdCommand root dir current)

makeRoot :: DirItem
makeRoot = Dir "/" []

parentDir :: DirItemIndex -> DirItemIndex
parentDir [] = error "root has no parent"
parentDir xs = init xs

contentDir :: DirItem -> [String]
contentDir f@File {} = [name f] -- maybe needs error
contentDir (Dir _ dirs) = map name dirs

isRoot :: DirItem -> Bool
isRoot File {} = False
isRoot dir = name dir == "/"

isName :: String -> DirItem ->  Bool
isName str f@File {} = name f == str
isName str dir  = name dir == str

dirItemSize :: DirItem -> Int
dirItemSize f@File {} = size f
dirItemSize (Dir _ []) = 0
dirItemSize dir = (sum . map dirItemSize) $ contents dir

dirSize :: DirItem -> Int
dirSize f@File {} = error"file dirSize is not possible"
dirSize (Dir _ []) = 0
dirSize dir | allFiles (contents dir) = dirItemSize dir
            | otherwise = dirItemSize dir + (sum . map dirSize) (filter isNotFile $ contents dir)

allFiles :: [DirItem] -> Bool
allFiles = all isNotFile

cdCommand :: DirItem -> String -> DirItemIndex -> DirItemIndex
cdCommand _ "/" _ = []
cdCommand _ ".." index = parentDir index
cdCommand dir str index = index ++ listOrNothing (findIndex (\x-> name x == str) (contents dir))-- oei buggs?

listOrNothing :: Maybe a -> [a]
listOrNothing Nothing = []
listOrNothing (Just a) = [a]

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

both :: (a -> Bool) -> a -> a -> Bool
both f a b = f a && f b

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