module Main where
import Data.Char (isDigit, intToDigit)
import Text.Read (readMaybe)
import Data.Maybe (isJust, fromJust, isNothing)
import GHC.Settings (maybeRead)


main :: IO ()
main = do
    input <- readFile "example_part2.txt"
    partOne input
    print . map ( \(x:xs)-> onlyChars [x] xs []) . lines $ input
    partTwo (partTwoArr input) 1
    print .  ("partTwo: " ++) . show . sum . partTwoArr $ input

partOne :: String -> IO()
partOne = print . ("partOne: " ++) . show . sum . map ( (\xs -> if null xs then 0 else read xs) . firstAndLast  . filter isDigit) . lines

partTwo :: [Int] -> Int -> IO()
partTwo [] _ = return ()
partTwo (x:xs) i = do
    print (show i ++ ": " ++ show x)
    partTwo xs (i+1)


partTwoArr :: String -> [Int]
partTwoArr = map ( (\xs -> if null xs then 0 else read xs) . firstAndLast . \(x:xs)-> onlyChars [x] xs []) . lines
-- .  ("partTwo: " ++) . show . sum
firstAndLast :: [a] -> [a]
firstAndLast []     = []
firstAndLast (x:xs) = if null xs then [x,x] else [x,last xs]

onlyChars :: String -> String -> [String] -> String
onlyChars current [] strs | isJust $ bruteMatchSingleInt current = [fromJust $ bruteMatchSingleInt current]
                          | otherwise = map (fromJust . bruteMatchSingleInt) $ filter (isJust . bruteMatchSingleInt) strs
onlyChars current@(x:xs) left@(y:ys) strs | isJust $ bruteMatchSingleInt current =
                                                fromJust (bruteMatchSingleInt current) : onlyChars (current ++ [y]) ys newStrs
                                          | otherwise = if not (null filtered) then filtered ++ onlyChars [y] ys (map ( ++ [y]) []) 
                                                                               else onlyChars (current ++ [y]) ys newStrs
    where
        filtered = map (fromJust . bruteMatchSingleInt) $ filter (isJust . bruteMatchSingleInt) strs
        newStrs = ([last current] : map ( ++ [last current]) strs)


bruteMatchSingleInt :: String -> Maybe Char
bruteMatchSingleInt str = case str of
    "one" -> Just '1'
    "two" -> Just '2'
    "three" -> Just '3'
    "four" -> Just '4'
    "five" -> Just '5'
    "six" -> Just '6'
    "seven" -> Just '7'
    "eight" -> Just '8'
    "nine" -> Just '9'
    x -> maybeRead x