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
type NodeMap = Map.Map String Node

nameN, leftN, rightN :: Node -> String
nameN v = v!!0
leftN v = v!!1
rightN v = v!!2

parseLine :: String -> Node
parseLine s = filter (not . null) $ split isSpace $ filter (not . (`elem` "=(,)")) s

toNodeMap :: [String] -> NodeMap
toNodeMap ss = Map.fromList $ (\v -> (nameN v, v)) . parseLine <$> ss

go :: NodeMap -> Char -> String -> Maybe String
go l d u = case d of
    'L' -> leftN <$> Map.lookup u l
    'R' -> rightN <$> Map.lookup u l

followAll :: NodeMap -> String -> Int -> [String] -> Int
followAll nodes directions index curr
    | all (\z -> z!!2 == 'Z') curr = 0
    | otherwise = 1 + followAll nodes directions ((index + 1) `mod` (length directions)) (mapMaybe (go nodes (directions !! index)) curr)

type Pos = (String, Int)

nodeP :: Pos -> String
nodeP = fst
indP :: Pos -> Int
indP = snd

go2 :: NodeMap -> String -> Pos -> Pos
go2 nodes directions u = (fromMaybe "" (go nodes (directions!!d) (nodeP u)), (d + 1) `mod` (length directions))
    where d = indP u

distance :: NodeMap -> String -> (Pos -> Bool) -> Pos -> Int
distance nodes directions u v
    | u v = 0
    | otherwise = 1 + distance nodes directions u (gogo v)
    where gogo = go2 nodes directions

collectZ :: NodeMap -> String -> Int -> (Pos -> Bool) -> Pos -> [(Pos, Int)]
collectZ nodes directions d u v
    | (nodeP v !! 2 == 'Z') && u v = [(v, d)]
    | u v = []
    | (nodeP v !! 2 == 'Z') = (v, d) : collectZ nodes directions (d+1) u (gogo v)
    | otherwise = collectZ nodes directions (d+1) u (gogo v)
    where gogo = go2 nodes directions

main :: IO ()
main = do
    input <- parseGroups <$> getContents
    let directions = head $ head input
    let graph = toNodeMap $ head (tail input)
    -- print $ collectZ graph directions 0 (\z -> nodeP z !! 2 == 'Z') ("BQA", 0)
    -- print $ collectZ graph directions 1 (== ("KPZ", 0)) (go2 graph directions ("KPZ", 0))
    print $ foldl lcm 1 $ distance graph directions (\z -> nodeP z !! 2 == 'Z') . (, 0) <$> filter (\z -> z!!2 == 'A') (Map.keys graph)