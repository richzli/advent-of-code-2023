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

data Range = Range {
    dst :: Int,
    src :: Int,
    len :: Int
} deriving (Show)

parseRange :: String -> Range
parseRange s = Range (ns!!0) (ns!!1) (ns!!2) where ns = parseNumbers s

inRange :: Range -> Int -> Bool
inRange r i = i >= src r && i < src r + len r

applyRange :: Range -> Int -> Int
applyRange r i = if inRange r i then dst r - src r + i else i

applyMap :: [Range] -> Int -> Int
applyMap rs i = case rs of
    [] -> i
    t:ts -> if inRange t i then applyRange t i else applyMap ts i

main :: IO ()
main = do
    input <- getContents
    let groups = parseGroups input
    let seeds = parseNumbers (split (==':') (head $ head groups) !! 1)
    let maps = fmap parseRange . tail <$> tail groups
    print $ minimum $ (\s -> foldl (flip applyMap) s maps) <$> seeds