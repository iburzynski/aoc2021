module Day2 where

type Position = (Int, Int)
type Command  = ([Char], Int)

main :: IO ()
main = do
    content <- readFile "input.txt"
    let p1 = uncurry (*) $ foldl move (0, 0) (fmtCommands content)
    let p2 = (\(p, _) -> uncurry (*) p) $
             foldl moveA ((0, 0), 0) (fmtCommands content)
    putStrLn $ "Part 1: " ++ show p1
    putStrLn $ "Part 2: " ++ show p2


fmtCommands :: [Char] -> [Command]
fmtCommands = map ((\[x, y] -> (x, read y) :: ([Char], Int)) . words) . lines


move :: Position -> Command -> Position
move (x, y) (d, n) = case d of
    "forward" -> (x + n, y)
    "down"    -> (x, y + n)
    "up"      -> (x, y - n)
    _         -> (x, y)


moveA :: (Position, Int) -> Command -> (Position, Int)
moveA ((x, y), a) (d, n) = case d of
    "forward" -> ((x + n, y + n * a), a)
    "down"    -> ((x, y), a + n)
    "up"      -> ((x, y), a - n)
    _         -> ((x, y), a)
