module Main where

import Data.Char

main :: IO ()
main = do
    input <- readFile "input.txt"
    let parsed = parseList input
    putStr "parsed\ntotal count: "
    print $ length parsed * length (head parsed)
    print $ partOne input
    print $ partTwo input



partOne :: String -> String
partOne = show . sum . map sum . checkAll checkRow. parseList

partTwo :: String -> String
partTwo = show . maximum . map maximum . checkAll scenicRow . parseList

scenicRow :: List2D -> ItemIndex -> [Int] -> [Int]
scenicRow list (x,y) result | y == length (head list) - 1 = sceneIndex list (x,y) : result
                            | otherwise = scenicRow list (x,y+1) (sceneIndex list (x,y) : result)

sceneIndex :: List2D -> ItemIndex -> Int
sceneIndex list index = sceneTop list index index 0 + sceneBottem list index index 0 + sceneLeft list index index 0 + sceneRight list index index 0

sceneTop :: List2D -> ItemIndex -> ItemIndex -> Int -> Int
sceneTop _ _ (0,_) result = result
sceneTop list (x,y) (v,h) result | (list!!x)!!y <= (list!!(v-1))!!h = sceneTop list (x,y) (v-1,h) (result+1)
                                 | otherwise = result

sceneBottem :: List2D -> ItemIndex -> ItemIndex -> Int -> Int
sceneBottem list (x,y) (v,h) result | v == length list -1 = result
                                    | (list!!x)!!y <= (list!!(v+1))!!h = sceneBottem list (x,y) (v+1,h) (result+1)
                                    | otherwise = result

sceneLeft :: List2D -> ItemIndex -> ItemIndex -> Int -> Int
sceneLeft _ _ (_,0) result = result
sceneLeft list (x,y) (v,h) result | (list!!x)!!y <= (list!!v)!!(h-1) = sceneLeft list (x,y) (v,h-1) (result + 1)
                                  | otherwise = result

sceneRight :: List2D -> ItemIndex -> ItemIndex -> Int -> Int
sceneRight list (x,y) (v,h) result | h == length (head list) - 1 = result
                                   | (list!!x)!!y <= (list!!v)!!(h+1) = sceneRight list (x,y) (v,h+1) (result +1)
                                   | otherwise = result

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