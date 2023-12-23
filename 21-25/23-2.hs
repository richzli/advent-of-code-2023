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

mulPoint :: Int -> Point -> Point
mulPoint c (x, y) = (c*x, c*y)

neighbors :: [Point] -> Point -> [Point]
neighbors d p = addPoint p <$> d

---

points :: Map.Map Point Char -> [Point]
points m = filter (\p -> let nbrs = mapMaybe ((`Map.lookup` m) . addPoint p) d4 in Map.lookup p m == Just '.' && (length nbrs == 3 || length (filter (=='#') nbrs) /= 2)) $ Map.keys m

getGraph :: Map.Map Point Char -> [Point] -> Map.Map Point (Map.Map Point Int)
getGraph m pts = Map.fromList $ zip pts $ go <$> pts
    where
        dfs v d q
            | Map.notMember q m = Map.empty
            | Map.lookup q m == Just '#' = Map.empty
            | Set.member q v = Map.empty
            | q `elem` pts = Map.singleton q d
            | otherwise = foldl Map.union Map.empty $ dfs (Set.insert q v) (d+1) <$> (addPoint q <$> d4)

        go p = foldl Map.union Map.empty $ dfs (Set.singleton p) 1 . addPoint p <$> d4

solve :: Point -> Point -> Map.Map Point (Map.Map Point Int) -> Int
solve src dst g = dfs Set.empty 0 src
    where
        dfs v d q
            | q == dst = d
            | Set.member q v = -1000000
            | otherwise = maximum $ uncurry (flip (dfs $ Set.insert q v)) <$> (Map.toList $ Map.map (+d) $ fromJust $ Map.lookup q g)

main :: IO ()
main = do
    input <- getContents
    let lines = parseLines input

    let n = length lines
    let m = length $ head lines
    let src = (0, 1)
    let dst = (n-1, m-2)

    let grid = Map.fromList $ enumerate2d lines
    let pts = points grid
    let g = getGraph grid pts
    print $ solve src dst g