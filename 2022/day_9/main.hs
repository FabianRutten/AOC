module Main where

import qualified Data.Set as Set

main :: IO ()
main = do
    input <- readFile "example.txt"
    print $ parseMotions input


partOne = undefined

parseMotions :: String -> [Motion]
parseMotions = map f . lines
    where
        f :: String -> Motion
        f line = q (head line) $ read $ tail line
        q 'L' = L
        q 'R' = R
        q 'U' = U
        q 'D' = D
        q _   = error "unknown motion"

iterateMotion :: Set.Set Loc -> Loc -> Loc -> Motion
iterateMotion = undefined

doMotionHead :: Loc -> Motion 
                -> (Loc ,Motion)
doMotionHead = undefined

checkMotionTail :: Set.Set Loc -> Loc -> Loc -> (Set.Set Loc , Loc)
checkMotionTail set head tail | closeToHead head tail = (set,tail)
                              | otherwise = (set,tail)

correctTail :: Loc -> Loc -> Loc
-- head -> tail -> tail
correctTail (x,y) (a,b) | x == a = (a,b+(y-b)) --move on y
                        | y == b = (a+(x-a),b) --move on x
                        | otherwise = (a+(x-a),b+(y-b)) -- move diagonal

closeToHead :: Loc -> Loc -> Bool
-- head -> tail -> bool
closeToHead (x,y) (a,b) = abs (x-a) <= 1 && abs (y-b) <= 1

type Loc = (Int,Int)
data Motion = L Int
            | R Int
            | U Int
            | D Int
    deriving Show
