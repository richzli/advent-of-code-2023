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

l1Point :: Point -> Int
l1Point (x, y) = abs x + abs y

neighbors :: [Point] -> Point -> [Point]
neighbors d p = addPoint p <$> d

---

expandRows :: [[Char]] -> [[Char]]
expandRows g
    | all null g = g
    | all ((=='.') . head) g = (".." ++) <$> expandRows (tail <$> g)
    | otherwise = zipWith (:) (head <$> g) $ expandRows (tail <$> g)

expandColumns :: [[Char]] -> [[Char]]
expandColumns g = case g of
    [] -> []
    g1:gs -> if all (=='.') g1 then g1:g1:expandColumns gs else g1:expandColumns gs

main :: IO ()
main = do
    input <- getContents
    let univ = expandColumns $ expandRows $ parseLines input
    let galaxies = fst <$> filter ((=='#') . snd) (enumerate2d univ)
    print $ (`div` 2) $ sum $ l1Point <$> (subPoint <$> galaxies <*> galaxies)