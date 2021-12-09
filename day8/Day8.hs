{-# LANGUAGE OverloadedStrings #-}

module Day8 where

import           Data.Char       (digitToInt, intToDigit)
import           Data.List       (sort)
import qualified Data.Map.Strict as M (Map, elems, empty, fromList, insert,
                                       insertWith, toList, union, (!), (!?))
import qualified Data.Text       as T (Text, length, lines, splitOn, unpack,
                                       words)
import qualified Data.Text.IO    as TIO (readFile)
import           Data.Tuple      (swap)


type Displays = M.Map Int [String]
type Circuits = M.Map Int Char
type Segment = T.Text
type Output = T.Text


main :: IO ()
main = do
    text <- TIO.readFile "input.txt"
    let lines = map ((\[x, y] -> (T.words x, T.words y)) . T.splitOn " | ") $ 
                T.lines text
        uniques = sum $ length . filter (\x -> length x == 1) .
                  map segments . snd <$> lines
    putStrLn $ "Part 1: " ++ show uniques
    putStrLn $ "Part 2: " ++ (show . sum $ map decode lines)


decode :: ([Segment], [Output]) -> Int
decode (x, os) = read $ map (\o -> intToDigit $ sol M.! unpackSort o) os
  where
    unpackSort = sort . T.unpack -- sort the string to use for lookup
    sol = solveCircuits x


solveCircuits :: [Segment] -> M.Map String Int
solveCircuits ts = swapMap . snd $ go (c, d) [0, 2, 5, 4, 6, 3, 1] --circ. order
  where
    (c, d) = (M.fromList $ zip [0..6] (repeat ' '), fillDisplays ts M.empty)
    go m [] = m
    go m (x:xs) = go (solveCircuit x m) xs


findElement :: (a -> Bool) -> [a] -> Maybe a
findElement f xs = case filter f xs of
  [x] -> Just x
  _   -> Nothing


swapMap :: M.Map Int [String] -> M.Map String Int
swapMap = M.fromList . fmap (swap . fmap (sort . concat)) . M.toList


getDisplay :: Displays -> Int -> String
getDisplay d i = concat $ d M.! i


-- Solves circuit letter for each position in [0..6]; solves displays when able
solveCircuit :: Int -> (Circuits, Displays) -> (Circuits, Displays)
solveCircuit i (c, d)
  | i >= 0 && i <= 6 = m
  | otherwise = (c, d)
  where
    alpha = ['a'..'g']
    updateC x y = case findElement (`notElem` x) y of
      Just circ -> M.insert i circ c
      Nothing -> c
    m = case i of
          0 -> let c' = updateC (getDisplay d 1) (getDisplay d 7)
               -- C0 : find letter in D7 not in D1, then solve D6
               in (c', solveDisplay 6 c' d)
          1 -> let c' = updateC (M.elems c) alpha
               -- C1 : find only remaining missing letter, then solve D2 and D3
               in (c', solveDisplay 3 c' $ solveDisplay 2 c' d)
          2 -> (updateC (getDisplay d 6) alpha, d)
               -- C2 : find letter not in D6
          3 -> (updateC (getDisplay d 0) alpha, d)
               -- C3 : find letter not in D0
          4 -> let c' = updateC ((c M.! 2) : getDisplay d 5) alpha
               -- C4 : find letter not in (C2 + D5), then solve D9
               in (c', solveDisplay 9 c' d)
          5 -> let c' = updateC [c M.! 0, c M.! 2] (getDisplay d 1)
               -- C5 : find letter in D1 that isn't C0 or C2, then solve D5
               in (c', solveDisplay 5 c' d)
          6 -> let c' = updateC ((c M.! 0) : (c M.! 4) : getDisplay d 4) alpha
               -- C6 : find letter not in (C0 + C4 + D4), then solve D0 
               in (c', solveDisplay 0 c' d)


-- Solve displays [0, 2, 3, 5, 6, 9]
solveDisplay :: Int -> Circuits -> Displays -> Displays
solveDisplay i c d = M.insert i [string] d
  where
    dStrings = d M.! i -- get list of possible strings for the display
    Just string = case i of
      0 -> findElement (`notElem` [getDisplay d 6, getDisplay d 9]) dStrings
           -- D0 is the length 6 string that doesn't match D6 or D9
      3 -> findElement (`notElem` [getDisplay d 2, getDisplay d 5]) dStrings
           -- D3 is the length 5 string that doesn't match D2 or D5
      6 -> findElement (\s -> not $ all (`elem` s) ((c M.! 0) : getDisplay d 1))
                       dStrings      
      _ -> let q = case i of
            -- identify which circuit letter to query
                    2 -> c M.! 5 -- D2 does not use C5
                    5 -> c M.! 2 -- D5 does not use C2
                    9 -> c M.! 4 -- D9 does not use C4
           in findElement (q `notElem`) dStrings


-- Set up initial displays
fillDisplays :: [Segment] -> M.Map Int [String]-> Displays
fillDisplays [] m = m
fillDisplays (s:ss) m = fillDisplays ss $ go (segments s) m (T.unpack s)
  where
    go [] m s = m
    go (x:xs) m s = go xs (M.insertWith (++) x [s] m) s


segments :: Segment -> [Int]
segments s =  case T.length s of
    2 -> [1]
    4 -> [4]
    3 -> [7]
    7 -> [8]
    5 -> [2, 3, 5]
    6 -> [0, 6, 9]
    _ -> []