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

data DsuNode = Root Int | Child String deriving (Eq, Ord, Show)
type Dsu = Map.Map String DsuNode

makeDsu :: [String] -> Dsu
makeDsu ss = Map.fromList $ (, Root 1) <$> ss

size :: Dsu -> String -> Int
size dsu v = case Map.lookup v dsu of
    Just (Root x) -> x
    Just (Child u) -> size dsu u

get :: Dsu -> String -> (Dsu, String)
get dsu v = case Map.lookup v dsu of
    Just (Root _) -> (dsu, v)
    Just (Child u) -> (Map.insert v (Child s) dsu, s)
        where
            (d, s) = get dsu u

merge :: Dsu -> String -> String -> Dsu
merge dsu u v = if a == b then dsu else d3
    where
        (d1, a) = get dsu u
        (d2, b) = get d1 v

        x = size dsu a
        y = size dsu b

        d3 = Map.insert a (Root (x+y)) $ Map.insert b (Child a) d2

---

type Graph = Map.Map String (Map.Map String Int)

addEdges :: Graph -> (String, [String]) -> Graph
addEdges g (u, vs) = Map.unionWith Map.union g m
    where
        m = foldl (\g' v -> Map.insert v (Map.singleton u 1) g') (Map.singleton u (Map.fromList $ (,1) <$> vs)) vs

parseLine :: String -> (String, [String])
parseLine s = (u, split isSpace vs)
    where
        [u, vs] = strip <$> split (==':') s

getEdge :: Graph -> String -> String -> Maybe Int
getEdge g u v = case Map.lookup u g of
    Just g' -> Map.lookup v g'
    Nothing -> Nothing

contract :: (Graph, Dsu) -> String -> String -> (Graph, Dsu)
contract (g, d) u v = (g', merge d2 u v)
    where
        (d1, a) = get d u
        (d2, b) = get d1 v

        g1 = Map.delete b $ Map.adjust (Map.delete b . Map.unionWith (+) (Map.delete a $ fromJust $ Map.lookup b g)) a g
        g' = Map.map (\m -> if Map.member b m then Map.delete b $ Map.insertWith (+) a (fromJust $ Map.lookup b m) m else m) g1

adjSearch :: Graph -> [String]
adjSearch g = search [v] m
    where
        v:vs = Map.keys g
        m = Map.fromList $ zip vs $ fromMaybe 0 . getEdge g v <$> vs

        search found rem | Map.null rem = found
        search found rem = search (f:found) (Map.mapWithKey (\k v -> v + fromMaybe 0 (getEdge g k f)) $ Map.delete f rem)
            where
                (c, f) = Map.foldlWithKey (\cur k v -> max cur (v, k)) (-1, "") rem

minCut :: Graph -> Dsu -> (Dsu, String)
minCut graph dsu = snd $ go graph dsu
    where
        go g d | Map.size g == 2 = (fromMaybe 0 $ getEdge g u v, (d, u))
            where
                [u, v] = Map.keys g
        go g d = min (go g' d') (wt, (d, s))
            where
                s:t:w = adjSearch g
                (g', d') = contract (g, d) s t
                wt = Map.foldl (+) 0 (fromJust $ Map.lookup s g)

main :: IO ()
main = do
    input <- getContents
    let g = foldl addEdges Map.empty $ parseLine <$> parseLines input
    let d = makeDsu $ Map.keys g
    let (res, v) = minCut g d
    let sz = size res v
    print $ sz * (Map.size d - sz)