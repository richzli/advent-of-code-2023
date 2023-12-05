import Data.Char
import Data.List
import System.IO

split :: (a -> Bool) -> [a] -> [[a]]
split p s = case break p s of
    (a, []) -> [a]
    (a, b) -> a : split p (drop 1 b)

parseLines :: String -> [String]
parseLines = split (=='\n')

parseGroups :: String -> [[String]]
parseGroups = split null . parseLines

getDigits :: String -> (Maybe Int, Maybe Int)
getDigits s = case s of
    "" -> (Nothing, Nothing)
    c:cs -> if isDigit c then (Just (read [c]), if null r then Just (read [c]) else r) else (l, r) where
        (l, r) = getDigits cs

calibrate :: (Maybe Int, Maybe Int) -> Int
calibrate (a, b) = case (a, b) of
    (Just x, Just y) -> 10 * x + y
    _ -> 0

main :: IO ()
main = do
    input <- getContents
    print $ sum $ calibrate . getDigits <$> parseLines input