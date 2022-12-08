module Main where

import Data.List ( nub )

main :: IO ()
main = do
    input <- readFile "input.txt"
    print $ partTwo input

partOne :: String -> Int
partOne = findMatch 4 1

partTwo :: String -> Int
partTwo = findMatch 14 1

findMatch :: Int -> Int -> String -> Int
findMatch n result [] = n + result
findMatch n result (x:xs) | predicate n xs = n + result
                        | otherwise = findMatch n (result + 1) xs

predicate :: Int -> String -> Bool
predicate i str = i == length (nub $ take i str)