module Day1 where

main :: IO ()
main = do
    content <- readFile "input.txt"
    let depths = map read $ lines content :: [Int]
    putStrLn $ "Part 1: " ++ show (depthIncreases depths)
    putStrLn $ "Part 2: " ++ show (windowIncreases 3 depths)


depthIncreases :: [Int] -> Int
depthIncreases = go 0
    where
        go acc [] = acc
        go acc [x] = acc
        go acc (x:x':xs) = let acc' = if x' > x then acc + 1 else acc
                           in go acc' (x':xs)


windowIncreases :: Int -> [Int] -> Int
windowIncreases w ds@(_:ds') = go 0 (sumWin ds) ds'
    where
        go acc prev ds@(_:ds')
            | length ds < w = acc
            | otherwise     = let acc' = if sumWin ds > prev then acc + 1 
                                         else acc
                              in go acc' (sumWin ds) ds'
        sumWin = sum . take w

