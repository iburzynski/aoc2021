{-# LANGUAGE TupleSections #-}

module Day9 where

import qualified Data.Text.IO as TIO
import qualified Data.Text as T
import Data.Char (digitToInt)
import Data.List (transpose, sortBy, elemIndices, nub)
import qualified Data.Map.Strict as M

type Graph = [[Int]]
type BoolGraph = [[Bool]]
type Point = (Int, Int)


main :: IO ()
main = do
    text <- TIO.readFile "input.txt"
    let lines = T.lines text
        graph = map (map digitToInt . T.unpack) lines :: [[Int]]
        graphT = transpose graph
        -- Check if each point is lower than its x-axis neighbors
        xs = map isLower graph
        -- Check if each point is lower than its y-axis neighbors
        ys = transpose $ map isLower graphT
        -- Check if each point is lower than *all* neighbors
        bools = bothTrue xs ys
        -- Get the low points in the grid to calculate their basin sizes
        lps = lowPoints bools
    -- Part 1: grid risk score
    putStrLn $ "Part 1: " ++ show (getRisk graph bools)
    -- Part 2: product of grid's 3 largest basins
    putStrLn $ "Part 2: " ++ show (threeBasinProd $ map (basinSize graph) lps)


-- /// PART 1 ///

-- Computes grid's risk score using its boolean counterpart
getRisk :: Graph -> BoolGraph -> Int
getRisk = go 0
  where
    go r [] _ = r
    go r _ [] = r
    go r (ds:dss) (bs:bss) =  go (r + go' 0 ds bs) dss bss 

    go' r [] _ = r
    go' r _ [] = r
    go' r (d:ds) (b:bs)
      | b = go' (r + d + 1) ds bs 
      | otherwise = go' r ds bs


-- Combines two boolean grids
bothTrue :: BoolGraph -> BoolGraph -> BoolGraph
bothTrue = go []
  where
    go acc [] _ = reverse acc
    go acc _ [] = reverse acc
    go acc (x:xs) (y:ys) = go (zipWith (&&) x y : acc) xs ys


-- Checks if each point is lower than its neighbors along a single axis. 
isLower :: [Int] -> [Bool]
isLower = go [] 9
  where
    go bs _ [] = []
    go bs p [x] = reverse ((x < p):bs)
    go bs p (x:y:zs)
      | x < p && x < y = go (True:bs) x (y:zs)
      | otherwise = go (False:bs) x (y:zs)


-- /// PART 2 ///

-- Calculates product of three largest basins
threeBasinProd :: [Int] -> Int
threeBasinProd bs
  | length bs < 3 = 0
  | otherwise = product . take 3 $ sortBy (flip compare) bs


-- Returns number of points within a given point's basin
basinSize :: Graph -> Point -> Int
basinSize g p = length $ basinPoints g [] [p]


-- Recursively generates set of unique basin points from initial singleton list 
basinPoints :: Graph -> [Point] -> [Point] -> [Point]
basinPoints g visited [] = nub visited
basinPoints g visited (p@(i, j) : ps) = go visited (p : ps)
  where
    u = (i - 1, j)
    d = (i + 1, j)
    l = (i, j - 1)
    r = (i, j + 1)

    go vs [] = vs
    go vs (p@(i, j):ps)
      | i > 0 &&
        g !! (i - 1) !! j /= 9 &&
        u `notElem` vs =
          basinPoints g (p : vs) (u : p : ps)
      | i < length g - 1 &&
        g !! (i + 1) !! j /= 9 &&
        d ` notElem` vs =
          basinPoints g (p : vs) (d : p : ps)
      | j > 0 &&
        g !! i !! (j - 1) /= 9 &&
        l `notElem` vs =
          basinPoints g (p : vs) (l : p : ps)
      | j < length (head g) - 1 &&
        g !! i !! (j + 1) /= 9 &&
        r `notElem` vs =
          basinPoints g (p : vs) (r : p : ps)
      | otherwise = basinPoints g (p : vs) ps


-- Returns indexed points for all True elements in a boolean grid
lowPoints :: BoolGraph -> [Point]
lowPoints = go 0 []
  where
    go i acc [] = concat acc
    go i acc (bs:bss) = go (i + 1) (map (i, ) (elemIndices True bs) : acc) bss