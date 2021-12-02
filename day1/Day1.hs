module Day1 where

main :: IO ()
main = do
    content <- readFile "input.txt"
    let depths = map read $ lines content :: [Int]
    putStrLn $ "Part 1: " ++ show (windowIncreases 1 depths)
    putStrLn $ "Part 2: " ++ show (windowIncreases 3 depths)


windowIncreases :: Int -> [Int] -> Int
windowIncreases w ds@(_:ds') = go 0 (sumWin ds) ds'
    where
        go acc _ [] = acc
        go acc prev ds@(_:ds')
            | length ds < w = acc
            | otherwise     = let acc' = if sumWin ds > prev then acc + 1 
                                         else acc
                              in go acc' (sumWin ds) ds'
        sumWin = sum . take w

