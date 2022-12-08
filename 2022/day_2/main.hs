module Main where

import Data.Char ( ord )

main :: IO ()
main = do
    input <- readFile "input.txt"
    print $  partTwo input

partOne :: String -> Int
partOne = foo calculateMatch

partTwo :: String -> Int
partTwo = foo predictMatch

foo ::  (Match -> Int) -> String -> Int
foo func = sum . map func . makeMatches

predictMatch :: Match -> Int
predictMatch (e, 'X') = (+) 0 $ giveP $ case e of
                        'A' -> 'Z'
                        'B' -> 'X'
                        'C' -> 'Y'
                        _   -> error "illegal match"
predictMatch (e, 'Y') = (+) 3 $ giveP $ case e of
                        'A' -> 'X'
                        'B' -> 'Y'
                        'C' -> 'Z'
                        _   -> error "illegal match"
predictMatch (e, 'Z') = (+) 6 $ giveP $ case e of
                        'A' -> 'Y'
                        'B' -> 'Z'
                        'C' -> 'X'
                        _   -> error "illegal match"
predictMatch _ = error "illegal match"

calculateMatch :: Match -> Int
calculateMatch m@(e,p) = giveP p + giveMatchResult m

giveP :: Char -> Int
giveP c = ord c - 87

giveMatchResult :: Match -> Int
giveMatchResult ('A',p) = case p of -- Rock
                            'X' -> 3
                            'Y' -> 6
                            'Z' -> 0
                            _ -> error "illegal match"
giveMatchResult ('B',p) = case p of -- Paper
                            'X' -> 0
                            'Y' -> 3
                            'Z' -> 6
                            _ -> error "illegal match"
giveMatchResult ('C',p) = case p of -- Scissor
                            'X' -> 6
                            'Y' -> 0
                            'Z' -> 3
                            _ -> error "illegal match"
giveMatchResult _ = error "illegal match"

makeMatches :: String -> [Match]
makeMatches = map splitOnWhite . lines

splitOnWhite :: String -> Match
splitOnWhite [h,_,t] = (h,t)
splitOnWhite _ = error "illegal splitOnWhite input"

type Match = (Char,Char)


