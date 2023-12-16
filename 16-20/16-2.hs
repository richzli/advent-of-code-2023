import Control.Applicative
import Control.Monad
import Control.Monad.State
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

reflect :: Int -> Char -> [Int]
reflect d m = case (d, m) of
    (_, '.') -> [d]
    (_, '\\') -> [d `xor` 1]
    (_, '/') -> [d `xor` 3]
    (a, '-') | a == 1 || a == 3 -> [d]
    (_, '-') -> [1, 3]
    (a, '|') | a == 0 || a == 2 -> [d]
    (_, '|') -> [0, 2]

type Tile = (Point, Int)
type BFSState = (Map.Map Point Char, Set.Set Tile, [Tile])

bfs :: State BFSState Int
bfs = do {
    (grid, visited, q) <- get;
    case q of
        [] -> return $ length $ filter (\x -> any (\y -> (x,y) `Set.member` visited) [0..3]) $ Map.keys grid
        t@(p, d):ts ->
            do { if t `Set.member` visited then
                put (grid, visited, ts)
            else
                case Map.lookup p grid of
                    Nothing -> put (grid, visited, ts)
                    Just x -> put (grid, Set.insert t visited, ts ++ ((\x -> (addPoint p $ d4!!x, x)) <$> reflect d x))
            ;
            bfs }
    }

main :: IO ()
main = do
    input <- getContents
    let lines = parseLines input
    let grid = Map.fromList $ enumerate2d lines
    let r = length lines - 1
    let c = length (head lines) - 1
    print $ maximum $ (\start -> evalState bfs (grid, Set.empty, [start])) <$> ((,0) . (0,) <$> [0..c]) ++ ((,1) . (,0) <$> [0..r]) ++ ((,2) . (r,) <$> [0..c]) ++ ((,3) . (,c) <$> [0..r])