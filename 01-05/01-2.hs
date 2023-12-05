import Control.Monad
import Data.Char
import Data.List
import Data.Maybe
import System.IO

split :: (a -> Bool) -> [a] -> [[a]]
split p s = case break p s of
    (a, []) -> [a]
    (a, b) -> a : split p (drop 1 b)

parseLines :: String -> [String]
parseLines = split (=='\n')

parseGroups :: String -> [[String]]
parseGroups = split null . parseLines

digits :: [(String, Int)]
digits = [
    ("zero", 0),
    ("one", 1),
    ("two", 2),
    ("three", 3),
    ("four", 4),
    ("five", 5),
    ("six", 6),
    ("seven", 7),
    ("eight", 8),
    ("nine", 9)
    ]

matchPrefix :: String -> String -> Bool
matchPrefix p s = isJust $ stripPrefix p s

matchDigit :: (String, Int) -> String -> Maybe Int
matchDigit (x, n) s = if matchPrefix x s then Just n else Nothing

matchDigits :: String -> [Maybe Int]
matchDigits s = case s of
    "" -> [Nothing]
    c:cs -> n : z where
        n = if isDigit c then Just (read [c]) else Nothing
        z = (`matchDigit` s) <$> digits

getDigits :: String -> (Maybe Int, Maybe Int)
getDigits s = case s of
    "" -> (Nothing, Nothing)
    c:cs -> if isJust x then (x, if isNothing r then x else r) else (l, r) where
        (l, r) = getDigits cs
        x = msum $ matchDigits s

calibrate :: (Maybe Int, Maybe Int) -> Int
calibrate (a, b) = case (a, b) of
    (Just x, Just y) -> 10 * x + y
    _ -> 0

main :: IO ()
main = do
    input <- getContents
    print $ sum $ calibrate . getDigits <$> parseLines input