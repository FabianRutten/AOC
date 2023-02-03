{-# LANGUAGE TupleSections #-}
module Main where

import Data.Char

main :: IO ()
main = do
    input <- readFile "input.txt"
    let parsed = parseList input
    putStr "parsed\ntotal count: "
    print $ length parsed * length (head parsed)
    print $ partOne input
    print $ part2 parsed

partOne :: String -> String
partOne = show . sum . map sum . checkAll checkRow. parseList

parseList :: String -> List2D
parseList = foldl f [] . reverse . lines
    where
        f :: List2D -> String -> List2D
        f xs str = map digitToInt str : xs

checkAll :: (List2D -> ItemIndex -> [Int] -> [Int]) -> List2D -> List2D
checkAll f list = func list 0 []
    where
        func :: List2D -> Int -> List2D -> List2D
        func list x result | x > length list - 1 = result
                           | otherwise =  f list (x,0) [] : func list (x+1) result

checkRow :: List2D -> ItemIndex -> [Int] -> [Int]
checkRow list (x,y) result | y == length (head list) - 1 = 1:result
                           | checkDirections list (x,y) = checkRow list (x,y+1) (1:result)
                           | otherwise = checkRow list (x,y+1) (0:result)

checkDirections :: List2D -> ItemIndex -> Bool
checkDirections list index = checkTop list index index || checkBottom list index index || checkLeft list index index || checkRight list index index

checkTop :: List2D -> ItemIndex -> ItemIndex -> Bool
checkTop _ _ (0,_) = True
checkTop list (x,y) (v,h) = (list!!x)!!y > (list!!(v-1))!!h && checkTop list (x,y) (v-1,h)

checkBottom :: List2D -> ItemIndex -> ItemIndex -> Bool
checkBottom list (x,y) (v,h) = v == length list -1 || ((list!!x)!!y > (list!!(v+1))!!h && checkBottom list (x,y) (v+1,h))

checkLeft :: List2D -> ItemIndex -> ItemIndex -> Bool
checkLeft _ _ (_,0) = True
checkLeft list (x,y) (v,h) = (list!!x)!!y > (list!!v)!!(h-1) && checkLeft list (x,y) (v,h-1)

checkRight :: List2D -> ItemIndex -> ItemIndex -> Bool
checkRight list (x,y) (v,h) = h == length (head list) - 1 || ((list!!x)!!y > (list!!v)!!(h+1) && checkRight list (x,y) (v,h+1))

printList :: List2D -> IO ()
printList ls = do mapM_ (putStrLn . concatMap show) ls

type ItemIndex = (Int,Int)
type List2D = [[Int]]

----part two

part2 :: List2D -> Int
part2 l = maximum $ map (scenicScore l) $ indexList l

indexList :: List2D -> [ItemIndex]
indexList l =  concatMap (\y -> map (,y) xs) ys
    where
        lengthX = maximum $ map length l
        lengthY = length l
        xs = [0..lengthX-1]
        ys = [0..lengthY-1]


scenicScore :: List2D -> ItemIndex -> Int
scenicScore l i = 
    let
        sT = scenicScoreTop l i
        sL = scenicScoreLeft l i
        sB = scenicScoreBottem l i
        sR = scenicScoreRight l i
    in
        sT * sL * sB * sR

scenicScoreTop :: List2D -> ItemIndex -> Int
scenicScoreTop = scenicScoreDirection (\(x,y)-> (x,y-1))

scenicScoreLeft :: List2D -> ItemIndex -> Int
scenicScoreLeft = scenicScoreDirection (\(x,y)-> (x-1,y))

scenicScoreBottem :: List2D -> ItemIndex -> Int
scenicScoreBottem = scenicScoreDirection (\(x,y)-> (x,y+1))

scenicScoreRight :: List2D -> ItemIndex -> Int
scenicScoreRight = scenicScoreDirection (\(x,y)-> (x+1,y))

scenicScoreDirection ::  (ItemIndex -> ItemIndex) -> List2D -> ItemIndex -> Int
scenicScoreDirection p l i = f 0 i
    where
        current = lookUp l i        
        maxY = length l
        maxX = maximum $ map length l
        f :: Int -> ItemIndex -> Int
        f result index@(x,y) | x <= 0 || y <= 0 = result
                             | x > maxX-2 || y > maxY-2 = result
                             | lookUp l (p index) >= current = result + 1
                             | otherwise = f (result + 1) $ p index

lookUp :: List2D -> ItemIndex -> Int
lookUp l (x,y) = (l!!x)!!y