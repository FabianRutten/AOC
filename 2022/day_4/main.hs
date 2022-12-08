module Main where
import Data.List.Split (splitOn)

main :: IO ()
main = do
    input <- readFile "input.txt"
    print $ partTwo input
    return ()

partTwo :: String -> Int
partTwo = foo overlapping

partOne :: String -> Int
partOne = foo containsOther

foo :: ((Range,Range) -> Bool) -> String -> Int
foo func = length . filter func . map makeRangeTuples . lines

makeRangeTuples :: String -> (Range,Range)
makeRangeTuples = (\xs -> (head xs, last xs)) . map ((\xs -> (read $ head xs, read $ last xs)) . splitOn "-") . splitOn ","

containsOther :: (Range,Range) -> Bool
containsOther ((l,h),(l1,h1)) = (l >= l1 && h <= h1 ) || (l <= l1 && h >= h1)

overlapping :: (Range,Range) -> Bool
overlapping ((l,h),(l1,h1)) = any (`elem` [l1..h1]) [l..h]

type Range = (Int,Int)