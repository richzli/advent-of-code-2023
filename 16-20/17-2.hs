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

--- (coords, direction, straightline)
type Tile = (Point, Int, Int)

next :: Tile -> [Tile]
next (p, d, s)
    | s == 0 = (\x -> (addPoint p $ d4 !! ((d + x) `mod` 4), (d + x) `mod` 4, 9)) <$> [1, 3]
    | s > 6 = [(addPoint p $ d4 !! d, d, s-1)]
    | otherwise = (addPoint p $ d4 !! d, d, s-1) : ((\x -> (addPoint p $ d4 !! ((d + x) `mod` 4), (d + x) `mod` 4, 9)) <$> [1, 3])

dijk :: [[Char]] -> Int
dijk grid =
    go Map.empty (Set.fromList [(-zz, ((0, 0), 0, 9)), (-zz, ((0, 0), 1, 9))])
    where
        n = length grid - 1
        m = length (head grid) - 1
        gridMap = Map.fromList $ (\(x, y) -> (x, (read::String->Int) [y])) <$> enumerate2d grid
        zz = fromJust $ Map.lookup (0, 0) gridMap
        go dist pq = case Set.minView pq of
            Nothing -> minimum $ mapMaybe (\(d, s) -> Map.lookup ((m, n), d, s) dist) ((,) <$> [0..3] <*> [0..6])
            Just ((d, t@(pt, dir, sl)), pq2) -> case (Map.lookup pt gridMap, Map.lookup t dist) of
                (Just d2, Nothing) -> go (Map.insert t (d+d2) dist) (Set.union pq2 $ Set.fromList $ (d+d2,) <$> next t)
                _ -> go dist pq2

main :: IO ()
main = do
    input <- getContents
    print $ dijk $ parseLines input