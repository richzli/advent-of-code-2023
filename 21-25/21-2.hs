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

go :: Map.Map Point Char -> Int -> Point -> Int
go grid i start = length $ iterate step (Set.singleton start) !! i
    where
        nbr p = filter ((`elem` [Just 'S', Just '.']) . (`Map.lookup` grid)) $ addPoint p <$> d4

        step ps = Set.foldl (\ns p -> Set.union ns $ Set.fromList (nbr p)) Set.empty ps

-- each copy is 131 x 131
-- checking that entire copy is traversable in 131 steps
floodfill :: Map.Map Point Char -> Int
floodfill grid = bfs Set.empty 0 [start]
    where
        start = head $ filter ((== Just 'S') . (`Map.lookup` grid)) $ Map.keys grid

        bfs v d [] = d
        bfs v d ps = let nv = foldr Set.insert v ps in
            bfs nv (d + 1) (filter (\p -> Set.notMember p nv && (Map.lookup p grid) `elem` [Just 'S', Just '.']) $ nub $ concatMap (\p -> addPoint p <$> d4) ps)

go2 :: Map.Map Point Char -> Int -> Point -> Int
go2 grid i start = bfs Set.empty i [start]
    where
        bfs v 0 ps = length ps
        bfs v d ps = let nv = foldr Set.insert v ps in
            (if d `mod` 2 == 0 then length ps else 0) + (bfs nv (d - 1) (filter (\p -> Set.notMember p nv && (Map.lookup p grid) `elem` [Just 'S', Just '.']) $ nub $ concatMap (\p -> addPoint p <$> d4) ps))

main :: IO ()
main = do
    input <- getContents
    let grid = Map.fromList $ enumerate2d $ parseLines input
    let start = head $ filter ((== Just 'S') . (`Map.lookup` grid)) $ Map.keys grid
    let sz = length $ parseLines input
    let halfsz = sz `div` 2
    let odds = go2 grid sz start
    let evens = go2 grid (sz+1) start

    let steps = 26501365
    let sqsteps = steps `div` sz
    let sqleft = steps `mod` sz

    let corners = [(0, 0), (0, sz-1), (sz-1, sz-1), (sz-1, 0)]
    let sides = [(0, halfsz), (sz-1, halfsz), (halfsz, sz-1), (halfsz, 0)]

    let fullAns = odds + (foldl (\acc i -> acc + 4 * i * (if i `mod` 2 == steps `mod` 2 then evens else odds)) 0 [1..sqsteps-1])
    let cornerAns = (sqsteps * (sum $ uncurry (go grid) . (sqleft-1,) <$> corners)) + ((sqsteps-1) * (sum $ uncurry (go2 grid) . (sqleft+sz-1,) <$> corners))
    let sideAns = sum $ uncurry (go2 grid) . (sqleft+halfsz,) <$> sides

    print $ fullAns + cornerAns + sideAns