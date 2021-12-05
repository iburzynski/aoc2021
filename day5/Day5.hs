{-# LANGUAGE OverloadedStrings #-}

module Day5 where

import qualified Data.Map.Strict as M
import qualified Data.Text       as T (Text, lines, pack, split, splitOn,
                                       unpack)

type Point = (Int, Int)
type Line = (Point, Point)


main :: IO ()
main = do
    input <- readFile "input.txt"
    let coords = map (T.splitOn " -> ") . T.lines $ T.pack input
        lines = map makeLine coords
        p1 = getCount . pointsMap $ concatMap getPoints1 lines
        p2 = getCount . pointsMap $ concatMap getPoints2 lines
    putStrLn $ "Part 1: " ++ show p1
    putStrLn $ "Part 2: " ++ show p2


makeLine :: [T.Text] -> Line
makeLine = (\[x, y] -> (x, y)). map makePoint


makePoint :: T.Text -> Point
makePoint = (\[x,y] -> (x, y)) . map (read . T.unpack) . T.split (== ',')


getCount :: M.Map a Int -> Int
getCount xs = length . M.keys $ M.filter (>= 2) xs


pointsMap :: [Point] -> M.Map Point Int
pointsMap = go M.empty
    where
        go acc []     = acc
        go acc (p:ps) = go (M.insertWith (+) p 1 acc) ps


-- No diagonals
getPoints1 :: Line -> [Point]
getPoints1 ((x, y), (x', y')) =
    case (xOrd, yOrd) of
        (EQ, LT) -> zip (repeat x) [y' .. y]
        (EQ, GT) -> zip (repeat x) [y .. y']
        (GT, EQ) -> zip [x .. x'] (repeat y)
        (LT, EQ) -> zip [x' .. x] (repeat y)
        _        -> []
    where
        (xOrd, yOrd) = (compare x' x, compare y' y)


-- Diagonals
getPoints2 :: Line -> [Point]
getPoints2 ((x, y), (x', y')) =
    case (xOrd, yOrd) of
        (EQ, LT) -> zip (repeat x) [y' .. y]
        (EQ, GT) -> zip (repeat x) [y .. y']
        (GT, EQ) -> zip [x .. x'] (repeat y)
        (LT, EQ) -> zip [x' .. x] (repeat y)

        (GT, LT) -> zip [x .. x'] [y, y - 1 .. y']
        (LT, LT) -> zip [x, x - 1 .. x'] [y, y - 1 .. y']
        (LT, GT) -> zip [x, x - 1 .. x'] [y .. y']
        (GT, GT) -> zip [x .. x'] [y .. y']

        _        -> [(x, y)]
    where
        (xOrd, yOrd) = (compare x' x, compare y' y)