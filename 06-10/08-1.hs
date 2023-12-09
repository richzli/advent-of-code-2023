import Control.Applicative
import Control.Monad
import Data.Bits
import Data.Char
import Data.List
import qualified Data.Map as Map
import Data.Maybe
import Data.Monoid
import qualified Data.Set as Set
import System.IO

--- string parsing ---

split :: (a -> Bool) -> [a] -> [[a]]
split p s = case break p s of
    (a, []) -> [a]
    (a, b) -> a : split p (drop 1 b)

strip :: String -> String
strip = dropWhile isSpace . dropWhileEnd isSpace

parseLines :: String -> [String]
parseLines = dropWhileEnd null . split (=='\n')

parseGroups :: String -> [[String]]
parseGroups = split null . parseLines

parseNumbers :: String -> [Int]
parseNumbers s = read <$> filter (/="") (split isSpace s)

--- utility ---

enumerate :: [a] -> [(Int, a)]
enumerate = zip [0..]

enumerate2d :: [[a]] -> [(Point, a)]
enumerate2d grid = [((x, y), c)
    | (x, row) <- enumerate grid
    , (y, c) <- enumerate row
    ]

--- coordinates ---

type Point = (Int, Int)

d4, d8 :: [Point]
d4 = [(1, 0), (0, 1), (-1, 0), (0, -1)]
d8 = [(1, 1), (1, 0), (1, -1), (0, 1), (0, -1), (-1, 1), (-1, 0), (-1, -1)]

zeroPoint :: Point
zeroPoint = (0, 0)

addPoint :: Point -> Point -> Point
addPoint (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

subPoint :: Point -> Point -> Point
subPoint (x1, y1) (x2, y2) = (x1 - x2, y1 - y2)

neighbors :: [Point] -> Point -> [Point]
neighbors d p = addPoint p <$> d

---

type Node = [String]

nameN, leftN, rightN :: Node -> String
nameN v = v!!0
leftN v = v!!1
rightN v = v!!2

parseLine :: String -> Node
parseLine s = filter (not . null) $ split isSpace $ filter (not . (`elem` "=(,)")) s

go :: [Node] -> Char -> String -> String
go [] _ _ = ""
go l d u
    | u == nameN v = case d of
        'L' -> leftN v
        'R' -> rightN v
    | otherwise = go vs d u
    where
        v = head l
        vs = tail l

follow :: [Node] -> String -> Int -> String -> Int
follow nodes directions index curr
    | curr == "ZZZ" = 0
    | otherwise = 1 + follow nodes directions ((index + 1) `mod` (length directions)) (go nodes (directions !! index) curr)

main :: IO ()
main = do
    input <- parseGroups <$> getContents
    let directions = head $ head input
    let graph = parseLine <$> head (tail input)
    print $ follow graph directions 0 "AAA"