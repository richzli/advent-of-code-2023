import Control.Applicative
import Control.Monad
import Data.Bits
import Data.Char
import Data.List
import qualified Data.Map as Map
import Data.Maybe
import Data.Monoid
import qualified Data.Sequence as Sequence
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

type PointMap a = Map.Map Point a

toPointMap :: [String] -> PointMap Char
toPointMap grid = Map.fromList $ enumerate2d grid

---
pNeighbors :: Char -> [Point]
pNeighbors c = case c of
    '|' -> [(-1, 0), (1, 0)]
    '-' -> [(0, -1), (0, 1)]
    'L' -> [(-1, 0), (0, 1)]
    'J' -> [(-1, 0), (0, -1)]
    '7' -> [(1, 0), (0, -1)]
    'F' -> [(1, 0), (0, 1)]
    'S' -> d4
    '.' -> []

type NextMap = PointMap [Point]

toNextMap :: PointMap Char -> NextMap
toNextMap pointMap = Map.mapMaybeWithKey (\p c -> Just (neighbors (pNeighbors c) p)) pointMap

bfs :: NextMap -> PointMap Int -> Sequence.Seq Point -> PointMap Int
bfs nbrs dis q
    | Sequence.null q = dis
    | otherwise = bfs nbrs (foldl (\m k -> Map.insert k (d+1) m) dis ns) (vs Sequence.>< Sequence.fromList ns)
        where
            (v Sequence.:<| vs) = q
            ns = filter (\x -> Map.findWithDefault (-1) x dis == (-1)) $ Map.findWithDefault [] v nbrs
            d = Map.findWithDefault (-1) v dis

getSPos :: PointMap Char -> Point
getSPos pointMap = head [x | x <- Map.keys pointMap, Map.lookup x pointMap == Just 'S']

pointGrid :: [[a]] -> [[Point]]
pointGrid g = zipWith (\x y -> (x,) <$> y) [0..] $ zipWith (curry fst) [0..] <$> g

countInside :: PointMap Char -> PointMap Int -> Int -> Maybe Char -> [Point] -> Int
countInside pointMap dists sum last indices
    | [] <- indices = 0
    | i:is <- indices, Just _ <- Map.lookup i dists = case (last, Map.lookup i pointMap) of
        (_, Just '|') -> cI (sum+1) last is
        (_, Just '-') -> cI sum last is
        (Nothing, Just 'L') -> cI sum (Just 'L') is
        (Nothing, Just 'F') -> cI sum (Just 'F') is
        (Just 'L', Just '7') -> cI (sum+1) Nothing is
        (Just 'F', Just 'J') -> cI (sum+1) Nothing is
        (Just _, Just _) -> cI sum Nothing is
        (_, _) -> cI sum last is
    | i:is <- indices = (sum `mod` 2) + cI sum last is
    where
        cI = countInside pointMap dists

main :: IO ()
main = do
    input <- getContents
    let lines = parseLines input
    let pointMap = toPointMap lines
    let nextMap = toNextMap pointMap
    let s = getSPos pointMap
    let sNbrs = filter (\x -> s `elem` Map.findWithDefault [] x nextMap) $ Map.findWithDefault [] s nextMap
    let sType = head $ filter (\c -> neighbors (pNeighbors c) s == sNbrs) "|-L7JF"
    -- print $ Map.foldr max 0 $ bfs nextMap (Map.fromList ((s, 0):((,1) <$> sNbrs))) (Sequence.fromList sNbrs)
    let dists = bfs nextMap (Map.fromList ((s, 0):((,1) <$> sNbrs))) (Sequence.fromList sNbrs)
    -- print $ filter (\x -> isNothing (Map.lookup (fst x) dists) && (snd x) `mod` 2 == 1) (enumerate2d $ tail . scanl (\x y -> x + (if isJust (Map.lookup y dists) && Map.lookup y pointMap == Just '|' then 1 else 0)) 0 <$> pointGrid lines)
    print $ sum $ countInside (Map.insert s sType pointMap) dists 0 Nothing <$> pointGrid lines