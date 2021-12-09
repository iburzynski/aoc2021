{-# LANGUAGE OverloadedStrings #-}

module Day7 where

import           Data.List    (sort)
import qualified Data.Map     as M (Map, empty, insertWith, toList)
import qualified Data.Text    as T (splitOn, unpack)
import qualified Data.Text.IO as TIO (readFile)


main :: IO ()
main = do
    text <- TIO.readFile "input.txt"
    let xs = sort <$> map (read . T.unpack) $ T.splitOn "," text :: [Int]
        Just (mean, len) = getMeanLen xs
        xs' = M.toList $ pointsMap xs
        med = (!!) xs $ len `div` 2
    print . sum $ map (\x -> abs $ x - med) xs
    print . minimum . map (getFuelCosts xs') $ [mean - 1 .. mean + 1]


pointsMap :: [Int] -> M.Map Int Int
pointsMap = foldr (\p q -> M.insertWith (+) p 1 q) M.empty


getMeanLen :: [Int] -> Maybe (Int, Int)
getMeanLen [] = Nothing
getMeanLen xs = let (sum, len) = foldr (\x (s, l) -> (s + x, l + 1)) (0,0) xs
                in Just (round $ toEnum sum / toEnum len, len)


getFuelCosts :: [(Int, Int)] -> Int -> Int
getFuelCosts xs m = sum $ map (\(p, q) -> (q *) . getFuelCost . abs $ p - m) xs
    where
        getFuelCost n = n * (n + 1) `div` 2