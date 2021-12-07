-- credit to Bartosz Milewski (https://github.com/BartoszMilewski/) for this solution!

{-# LANGUAGE OverloadedStrings #-}

module Day6 where

import qualified Data.Map.Strict as M (Map, adjust, elems, fromList, insert,
                                       insertWith)
import qualified Data.Text       as T (splitOn, unpack)
import qualified Data.Text.IO    as TIO (readFile)

type FishPop = M.Map Int Int


main :: IO ()
main = do
    text <- TIO.readFile "input.txt"
    let fish = map (read . T.unpack) $ T.splitOn "," text :: [Int]
        spawns = iterate spawn (makeStartPop fish)
    putStrLn $ "Part 1: " ++ (show . sum $ spawns !! 80)
    putStrLn $ "Part 2: " ++ (show . sum $ spawns !! 256)


makeStartPop :: [Int] -> FishPop
makeStartPop = let p0 = M.fromList $ zip [0..8] (repeat 0)
               in foldr (\fish pop -> M.insertWith (+) fish 1 pop) p0


spawn :: FishPop -> FishPop
spawn p = M.insert 8 spawning aged
    where
        (spawning : others) = M.elems p
        aged = M.adjust (+ spawning) 6 . M.fromList $ zip [0..7] others
