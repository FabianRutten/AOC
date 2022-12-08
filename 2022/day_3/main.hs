module Main where

import Data.Char ( ord )
import Data.List ( find )
import Data.Maybe ( fromJust)
import Data.List.Split ( chunksOf )

main :: IO ()
main = do
    input <- readFile "input.txt"
    print $ partTwo input


partOne :: String -> Int
partOne = sum . map (getPrio . evalRucksackString) . lines

partTwo :: String -> Int 
partTwo = sum . map (getPrio . evalGroup) . chunksOf 3 . lines

evalGroup :: [String] -> Char
evalGroup = fromJust . (\[a,b,c] -> find (\x-> (x `elem` b) && (x `elem` c)) a)

evalRucksackString :: String -> Char
evalRucksackString = (\(x,y) -> fromJust $ find (`elem` y ) x) . (\x-> splitAt (length x `div` 2) x)

getPrio :: Char -> Int
getPrio c = let o = ord c - 96
            in if o < 0 then o + 58 else o

type Rucksack = (String,String)