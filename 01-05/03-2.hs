import Control.Applicative
import Control.Monad
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

neighbors :: [Point] -> Point -> Map.Map Point (Set.Set Point)
neighbors d p = Map.fromList $ (, Set.singleton p) <$> (addPoint p <$> d)

--- utility ---

enumerate :: [a] -> [(Int, a)]
enumerate = zip [0..]

enumerate2d :: [[a]] -> [(Point, a)]
enumerate2d grid = [((x, y), c)
    | (x, row) <- enumerate grid
    , (y, c) <- enumerate row
    ]

---

isPart :: Char -> Bool
isPart c = not (isDigit c) && (c /= '.')

isGear :: Char -> Bool
isGear = (=='*')

getParts :: [[Char]] -> [Point]
getParts grid = [ p | (p, c) <- enumerate2d grid, isGear c ]

getNeighbors :: [Point] -> Map.Map Point (Set.Set Point)
getNeighbors ps = foldl (Map.unionWith Set.union) Map.empty $ neighbors d8 <$> ps

nextDigit :: Maybe Int -> Int -> Maybe Int
nextDigit x d = case x of
    Just x -> Just (10 * x + d)
    Nothing -> Nothing

joinIf :: Ord k => Maybe (Set.Set k) -> Set.Set k -> Set.Set k
joinIf t s = case t of
    Just u -> Set.union u s
    Nothing -> s

parseRow :: Map.Map Point (Set.Set Point) -> [(Point, Char)] -> ((Maybe Int, Set.Set Point), [(Maybe Int, Set.Set Point)])
parseRow nbrs row
    | null row = ((Nothing, Set.empty), [])
    | isNothing x && isDigit c = ((Just (read [c]), joinIf (Map.lookup p nbrs) Set.empty), xs)
    | isJust x && isDigit c = ((nextDigit x $ read [c], joinIf (Map.lookup p nbrs) b), xs)
    | not $ isDigit c = ((Nothing, Set.empty), (x, b) : xs)
    where
        (p, c) = head row
        ((x, b), xs) = parseRow nbrs $ tail row

fixRow :: Int -> [(Int, a)] -> [(Point, a)]
fixRow r l = reverse [ ((r, y), c)
    | (y, c) <- l
    ]

parseGrid :: [[Char]] -> [(Int, Set.Set Point)]
parseGrid grid =
    [ (x, b)
    | ((xx, bb), xs) <- parseRow ps . uncurry fixRow <$> enumerate (enumerate <$> grid)
    , (Just x, b) <- (xx, bb) : xs
    , not $ null b
    ]
    where
        ps = getNeighbors $ getParts grid

insertListMap :: Ord k => (k, v) -> Map.Map k [v] -> Map.Map k [v]
insertListMap (p, n) = Map.insertWith (++) p [n]

test :: [(Int, Set.Set Point)] -> [(Point, [Int])]
test g = [(p, [n]) | (n, ps) <- g, p <- Set.toList ps]

gearRatios :: [(Int, Set.Set Point)] -> Map.Map Point [Int]
gearRatios g = foldr insertListMap Map.empty [(p, n) | (n, ps) <- g, p <- Set.toList ps]

comb :: [Int] -> Int -> Int
comb l a
    | length l == 2 = a + product l
    | otherwise = a

main :: IO ()
main = do
    input <- getContents
    let grid = parseLines input
    print $ Map.foldr comb 0 (gearRatios $ parseGrid grid)