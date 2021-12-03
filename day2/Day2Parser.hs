-- Solution using parser from scratch
module Day2Parser where

import           Control.Applicative (Alternative (empty, many, (<|>)))
import           Data.Char           (isDigit, isSpace)
import           Data.Tuple          (swap)


data Direction = Forward | Up | Down
    deriving (Show, Eq)


data Command = Command Direction Int
    deriving (Show, Eq)


data Position = Position Int Int
    deriving (Show, Eq)


newtype Parser a = Parser
    { runParser :: String -> Maybe (String, a)
    }


instance Functor Parser where
    -- fmap :: (a -> b) -> f a -> f b
    fmap f (Parser p) = Parser $ \input -> do
        (s, a) <- p input
        pure (s, f a)


instance Applicative Parser where
    -- pure :: Applicative f => a -> f a
    pure x = Parser (\input -> Just (input, x))
    -- (<*>) :: Applicative f => f (a -> b) -> f a -> f b
    (Parser p1) <*> (Parser p2) = Parser $ \input -> do
       (s, f) <- p1 input
       (s', x) <- p2 s
       pure (s', f x)


instance Alternative Parser where
    empty = Parser $ const Nothing
    (Parser p1) <|> (Parser p2) = Parser $ \input -> p1 input <|> p2 input


main :: IO ()
main = do
    content <- readFile "input.txt"
    let Just (Position x y, (Position x' y', _)) = do
        (_, cs) <- runParser commands content
        pure (foldl move (Position 0 0) cs, foldl moveA (Position 0 0, 0) cs)
    putStrLn $ "Part 1: " ++ show (x * y)
    putStrLn $ "Part 2: " ++ show (x' * y')


move :: Position -> Command -> Position
move (Position x y) (Command d n) = case d of
    Forward -> Position (x + n) y
    Down    -> Position x (y + n)
    Up      -> Position x (y - n)


moveA :: (Position, Int) -> Command -> (Position, Int)
moveA (Position x y, a) (Command d n) = case d of
    Forward -> (Position (x + n) (y + n * a), a)
    Down    -> (Position x y, a + n)
    Up      -> (Position x y, a - n)


-- Basic Parsers
charP :: Char -> Parser Char
charP x = Parser f
    where
        f (y:ys)
            | y == x = Just (ys, x)
            | otherwise = Nothing
        f [] = Nothing


stringP :: String -> Parser String
stringP = traverse charP


spanP :: (Char -> Bool) -> Parser String
spanP f = Parser $ \input -> Just . swap $ span f input


notNull :: Parser [a] -> Parser [a]
notNull (Parser p) = Parser $ \input -> do
    (input', xs) <- p input
    if null xs
        then Nothing
        else pure (input', xs)


ws :: Parser String
ws = spanP isSpace


-- Puzzle Parsers
direction :: Parser (Int -> Command)
direction = f <$> (stringP "forward" <|> stringP "up" <|> stringP "down")
    where
        f "forward" = Command Forward
        f "up"      = Command Up
        f "down"    = Command Down
        f _         = undefined


distance :: Parser Int
distance = read <$> notNull (spanP isDigit)


command :: Parser Command
command = direction <* ws <*> distance


commands :: Parser [Command]
commands = many (command <* ws)
