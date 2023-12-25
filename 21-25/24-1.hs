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

solveMat :: [Int] -> [Int] -> [Int] -> [Int] -> Maybe [Float]
solveMat [x1, y1] [vx1, vy1] [x2, y2] [vx2, vy2] = case det of
    0 -> Nothing
    _ -> Just [fromIntegral (dy * vx2 - dx * vy2) / det, fromIntegral (dy * vx1 - dx * vy1) / det]
    where
        det = fromIntegral (vx1 * (-vy2) - (-vx2) * vy1)
        dx = x2 - x1
        dy = y2 - y1

type Hail = ([Int], [Int])

check :: Hail -> Hail -> Maybe [Float]
check (x1, v1) (x2, v2) = solveMat x1 v1 x2 v2

bound :: Hail -> Maybe [Float] -> Bool
bound (x, v) mt = case mt of
    Nothing -> False
    Just t -> all (>=0) t &&
              all (\a -> a >= 200000000000000 && a <= 400000000000000) (zipWith (+) xx $ (*(head t)) <$> vv)
        where
            xx = fromIntegral <$> x
            vv = fromIntegral <$> v

go :: Hail -> Hail -> Bool
go h1 h2 = bound h1 (check h1 h2)

parseLine :: String -> Hail
parseLine s = (init $ readCoords x, init $ readCoords v)
    where
        [x, v] = strip <$> split (=='@') s
        readCoords l = read . strip <$> split (==',') l

main :: IO ()
main = do
    input <- getContents
    let hails = parseLine <$> parseLines input
    print $ (`div` 2) $ length $ filter id $ go <$> hails <*> hails