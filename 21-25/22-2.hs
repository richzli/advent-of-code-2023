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

divPoint :: Int -> Point -> Point
divPoint c (x, y) = (x `div` c, y `div` c)

neighbors :: [Point] -> Point -> [Point]
neighbors d p = addPoint p <$> d

dotPoint :: Point -> Point -> Int
dotPoint (x1, y1) (x2, y2) = x1 * x2 + y1 * y2

crossPoint :: Point -> Point -> Int
crossPoint (x1, y1) (x2, y2) = x1 * y2 - x2 * y1

crossPoint3 :: Point -> Point -> Point -> Int
crossPoint3 p a b = crossPoint (subPoint a p) (subPoint b p)

onSegment :: Point -> Point -> Point -> Bool
onSegment s e p = crossPoint3 p s e == 0 && dotPoint (subPoint s p) (subPoint e p) <= 0

segInter :: Point -> Point -> Point -> Point -> Bool
segInter a b c d = or [(signum oa) * (signum ob) < 0 && (signum oc) * (signum od) < 0, onSegment c d a, onSegment c d b, onSegment a b c, onSegment a b d]
    where
        oa = crossPoint3 c d a
        ob = crossPoint3 c d b
        oc = crossPoint3 a b c
        od = crossPoint3 a b d
---

type Brick = [[Int]]

overlap :: Brick -> Brick -> Bool
overlap [[x1, y1, z1], [x2, y2, z2]] [[x3, y3, z3], [x4, y4, z4]] = z1 > z4 && segInter (x1, y1) (x2, y2) (x3, y3) (x4, y4)

support :: Brick -> Brick -> Bool
support b ref = overlap b ref && (b !! 0 !! 2) == (ref !! 1 !! 2) + 1

fall :: Brick -> Brick -> Int
fall b ref = (b !! 0 !! 2) - 1 - if overlap b ref then (ref !! 1 !! 2) else 0

down :: Int -> Brick -> Brick
down i [[x1, y1, z1], [x2, y2, z2]] = [[x1, y1, z1-i], [x2, y2, z2-i]]

nullBrick :: Brick
nullBrick = [[-1, -1, -1], [-1, -1, -1]]

bfs :: (Ord a) => Map.Map a [a] -> Map.Map a [a] -> a -> Int
bfs m mp b = go Set.empty [b] - 1
    where
        go v [] = Set.size v
        go v ps = go nv (nub $ filter (all (`Set.member` nv) . fromJust . (`Map.lookup` mp)) $ concat $ mapMaybe (`Map.lookup` m) ps)
            where
                nv = Set.union v $ Set.fromList ps

main :: IO ()
main = do
    input <- getContents
    let bricks = sortBy (\b1 b2 -> compare (b1 !! 0 !! 2) (b2 !! 0 !! 2)) $ fmap (fmap (read::String->Int) . split (==',')). split (=='~') <$> parseLines input
    let dropped = foldl (\l b -> (down (minimum $ fall b <$> nullBrick:l) b) : l) [] bricks
    let g = Map.fromList $ zip dropped $ (\b -> filter (`support` b) dropped) <$> dropped
    let gp = Map.fromList $ zip dropped $ (\b -> filter (b `support`) dropped) <$> dropped
    -- print $ Map.size $ Map.filter (not . any ((== Just 1) . (\b -> length <$> Map.lookup b gp))) g
    print $ sum $ bfs g gp <$> dropped