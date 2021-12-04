module Day4 where
import           Data.Char (digitToInt)
import           Data.List (transpose)
import qualified Data.Text as T (Text, pack, split, unpack)

type Row = [Int]
type Board = [Row]

main :: IO ()
main = do
    input <- readFile "input.txt"
    let content = T.split (=='\n') $ T.pack input
        ds = map (read . T.unpack) . T.split (==',') . head $ content :: [Int]
        boards = formatBoards $ tail content
    putStrLn $ "Part 1: " ++ show (gameWin ds boards)
    putStrLn $ "Part 2: " ++ show (gameLose ds boards)


formatBoards :: [T.Text] -> [Board]
formatBoards = go []
    where
        go acc [] = acc
        go acc (x:xs)
            | x == T.pack "" = go acc xs
            | otherwise =
                let board = map formatRow (take 5 (x:xs))
                in go (board : acc) (drop 5 (x:xs))


formatRow :: T.Text -> Row
formatRow x = [ read y | y <- map T.unpack $ T.split (== ' ') x, y /= "" ]


markRow :: Int -> Row -> Row
markRow n = map (\x -> if x == n then (-1) else x)


markBoard :: Int -> Board -> Board
markBoard n = map (markRow n)


winner :: Row -> Bool
winner = all (== (-1))


checkBoard :: Board -> Bool
checkBoard rs = any winner rs || any winner (transpose rs)


scoreRow :: Row -> Int
scoreRow = sum . filter (/= (-1))


scoreBoard :: Board -> Int
scoreBoard = sum . map scoreRow


gameWin :: [Int] -> [Board] -> Int
gameWin [] _ = 0
gameWin (d:ds) bs = case playToWin d bs of
    Right n  -> n * d
    Left bs' -> gameWin ds bs'


gameLose :: [Int] -> [Board] -> Int
gameLose [] _      = 0
gameLose ds [b]    = gameWin ds [b]
gameLose (d:ds) bs = gameLose ds (playToLose d bs)


playToWin :: Int -> [Board] -> Either [Board] Int
playToWin d = go []
    where
        go acc [] = Left acc
        go acc (b:bs) =
            let newBoard = markBoard d b
            in
                if checkBoard newBoard then Right $ scoreBoard newBoard
                else go (newBoard : acc) bs


playToLose :: Int -> [Board] -> [Board]
playToLose d = go []
    where
        go acc [] = acc
        go acc (b:bs) =
            let newBoard = markBoard d b
            in
                if checkBoard newBoard then go acc bs
                else go (newBoard : acc) bs
