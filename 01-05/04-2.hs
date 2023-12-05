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

checkCard :: [Int] -> [Int] -> Int
checkCard w n = length $ filter (`elem` w) n

parseNumbers :: String -> [Int]
parseNumbers s = read <$> filter (/="") (split isSpace s)

parseCard :: String -> Int
parseCard s =
    checkCard (parseNumbers w) (parseNumbers n)
    where
        cinfo:ns:_ = strip <$> split (==':') s
        w:n:_ = strip <$> split (=='|') ns

score :: Int -> Int
score x
    | 0 <- x = 0
    | otherwise = 1 `shiftL` (x - 1)

---

sumUp :: Int -> Int -> [Int] -> [Int]
sumUp n x l
    | 0 <- n = l
    | [] <- l = replicate n x
    | y:ys <- l = (x + y) : sumUp (n - 1) x ys

solve :: [Int] -> [Int] -> Int
solve dp l = case l of
    [] -> sum dp
    y:ys -> 1 + h + solve (sumUp y (1 + h) t) ys
    where
        h = if null dp then 0 else head dp
        t = if null dp then [] else tail dp

main :: IO ()
main = do
    input <- getContents
    -- print $ sum $ score . parseCard <$> parseLines input
    print $ solve [] $ parseCard <$> parseLines input