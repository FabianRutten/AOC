module Main where

import qualified Data.Set as Set

main :: IO ()
main = do
    input <- readFile "example.txt"
    print $ parseMotions input
    print $ partOne input


partOne :: String -> Int
partOne = Set.size . doAllMotions (0,0) (0,0) . parseMotions

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

doAllMotions ::Pos -> Pos ->  [Motion] -> Set.Set Pos
doAllMotions head tail = (\(set,_,_)-> set) . flip f (Set.singleton tail, head, tail)
    where
        f [] result     = result
        f (x:xs) result = f xs $ iterateMotion result x

iterateMotion :: (Set.Set Pos ,  Pos , Pos )-> Motion -> (Set.Set Pos ,Pos, Pos)
iterateMotion (set , head , tail) motion | motionEmpty motion = (set,head,tail)
                                   | otherwise          = iterateMotion (newSet , newHead , newTail) newMotion
    where
        (newHead,newMotion) = doMotionHead head motion
        (newSet, newTail)   = checkMotionTail set newHead tail

doMotionHead :: Pos -> Motion -> (Pos , Motion)        
doMotionHead (x,y) (L i) = ((x-i,y),L (i-1))
doMotionHead (x,y) (R i) = ((x+i,y),R (i-1))
doMotionHead (x,y) (U i) = ((x,y-i),U (i-1))
doMotionHead (x,y) (D i) = ((x,y+1),D (i-1))

checkMotionTail :: Set.Set Pos -> Pos -> Pos -> (Set.Set Pos , Pos)
checkMotionTail set head tail | closeToHead head tail = (set,tail)
                              | otherwise = (Set.insert newTail set, newTail)
    where
        newTail = correctTail head tail

            -- head -> tail -> tail
correctTail :: Pos -> Pos -> Pos
correctTail (x,y) (a,b) | x == a    = (a,fb)
                        | y == b    = (fa,b)
                        | otherwise = (fa,fb)
    where
        fa = let m = if a > x then 1 else (-1) in a+(x-a) + (m * fst q)
        fb = let m = if b > x then 1 else (-1) in b+(y-b) + (m * snd q)
        q  | abs (a-x) > abs (b-y) = (1,0)
           | otherwise = (0,1)

            -- head -> tail -> bool
closeToHead :: Pos -> Pos -> Bool
closeToHead (x,y) (a,b) = abs (x-a) <= 1 && abs (y-b) <= 1

type Pos = (Int,Int)
data Motion = L Int
            | R Int
            | U Int
            | D Int
    deriving Show

motionEmpty :: Motion -> Bool
motionEmpty (L i) = i == 0
motionEmpty (R i) = i == 0
motionEmpty (U i) = i == 0
motionEmpty (D i) = i == 0

