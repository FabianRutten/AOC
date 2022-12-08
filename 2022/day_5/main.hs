module Main where
import Data.List.Split ( splitOn, chunksOf)
import Data.List (transpose, groupBy)
import Data.Char (isDigit,isAlpha)


main :: IO ()
main = do
    input <- readFile "input.txt"
    print $ partTwo input

partOne :: String -> String
partOne = foo reverse

partTwo :: String -> String
partTwo = foo id

foo :: (String -> String) -> String -> String
foo f str = let (towerString, moveString) = splitStacksAndMovement str
              in concatMap (return . head) (doMoves f (toMoveList moveString) (toTowerLists towerString))

splitStacksAndMovement :: String -> (String,String)
splitStacksAndMovement = (\xs-> (head xs, last xs)). splitOn "\n\n"

toTowerLists :: String -> [Tower]
toTowerLists = filter (not . null) . map (filter isAlpha) . transpose . lines

toMoveList :: String -> [Move]
toMoveList = map ((\[a,b,c]-> (a,b,c)) . map read . (filter (any isDigit) . groupBy (\x y-> isDigit y && isDigit x))) . lines

doMoves :: (String -> String) -> [Move] -> [Tower] -> [Tower]
doMoves f xs ys = foldl (doMove f) ys xs

doMove :: (String -> String) -> [Tower] -> Move -> [Tower]
doMove f tw (n, i, j) = (replace (f out++(tw!!(j-1))) (j-1) . replace stay (i-1)) tw
    where
        (out,stay) = splitAt n $ tw!!(i-1)

replace :: a -> Int -> [a] -> [a]
replace x _ [] = [x]
replace x i xs = left ++ x: tail ys
    where
        (left,ys) = splitAt i xs


type Tower = [Char]
type Move = (Int,Int,Int)

