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

matchRowFlip :: [String] -> Int -> Bool
matchRowFlip grid i = and $ zipWith (==) (reverse t) b
    where
        (t, b) = splitAt i grid

match :: [String] -> Int
match g = head $ rowMatches ++ colMatches
    where
        rowMatches = (* 100) <$> filter (matchRowFlip g) [1..(length g - 1)]
        colMatches = filter (matchRowFlip (transpose g)) [1..(length (head g) - 1)]

smudge :: Char -> Char
smudge c = case c of
    '#' -> '.'
    '.' -> '#'

matchRowFlip2 :: [String] -> Int -> Int
matchRowFlip2 grid i = if and $ zipWith (==) (reverse t) b then i else 0
    where
        (t, b) = splitAt i grid

smudgeRow :: String -> [String]
smudgeRow "" = []
smudgeRow (c:cs) = (smudge c:cs) : ((c:) <$> smudgeRow cs)

smudgeGrid :: [String] -> [[String]]
smudgeGrid [] = []
smudgeGrid (r:rs) = ((:) <$> (smudgeRow r) <*> [rs]) ++ ((r:) <$> smudgeGrid rs)

matchSmudges :: ([String], Int) -> Int
matchSmudges (g, orig) = sum $ nub $ filter (/= orig) $ rowMatches ++ colMatches
    where
        r = length g
        c = length (head g)
        rowMatches = (* 100) <$> (matchRowFlip2 <$> smudgeGrid g <*> [1..(r-1)])
        colMatches = matchRowFlip2 <$> smudgeGrid (transpose g) <*> [1..(c-1)]

main :: IO ()
main = do
    input <- getContents
    let patterns = parseGroups input
    let original = zip patterns (match <$> patterns)
    print $ sum $ matchSmudges <$> original