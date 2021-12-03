module Day3 where

import           Data.Char (digitToInt, intToDigit)
import           Data.List (transpose)

type Gamma = Int
type Epsilon = Int


main :: IO ()
main = do
    content <- readFile "input.txt"
    let
        digits = [ map digitToInt ds | ds <- words content ]
        transposed = transpose digits
        (g, e) = unzip (map counts transposed)
    putStrLn $ "Part 1: " ++ 
               (show $ binToDec g * binToDec e)
    putStrLn $ "Part 2: " ++ 
               (show $ getRating gamma digits * getRating epsilon digits)


counts :: [Int] -> (Gamma, Epsilon)
counts xs = case compare (sum xs) (length xs - sum xs) of
    GT -> (1, 0)
    LT -> (0, 1)
    EQ -> (1, 0)


binToDec :: [Int] -> Int
binToDec = foldl ((+) . (2*)) 0


getRating :: ([[Int]] -> Int -> Int) -> [[Int]] -> Int
getRating = go 0
    where
        go _ _ []  = -1
        go _ _ [x] = binToDec x
        go i f xs  = go (i + 1) f (filter (\x -> x !! i == f xs i) xs)


countsAt :: [[Int]] -> Int -> (Gamma, Epsilon)
countsAt xs = counts . (!!) (transpose xs)


gamma :: [[Int]] -> Int -> Gamma
gamma xs = fst . countsAt xs


epsilon :: [[Int]] -> Int -> Epsilon
epsilon xs = snd . countsAt xs


