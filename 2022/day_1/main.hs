module Main where
import Data.List

main :: IO ()
main = do
    input <- readFile "input.txt"
    print $ partTwo input -- partOne for part one awnser

partOne :: String -> Int
partOne str = maximum $ map sum $ elves str

partTwo :: String -> Int
partTwo str = sum $ take 3 $ sortedBymax summed
    where
        sortedBymax = reverse . sort
        summed = map sum $ elves str

elves :: String -> [[Int]]
elves str = map (map read . filter (/= "")) $ groupBy (\_ y -> y /= "") $ lines str