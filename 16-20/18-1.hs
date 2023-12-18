import Control.Applicative
import Control.Monad
import Control.Monad.State
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

type Instr = (Point, Int, String)
dirI :: Instr -> Point
dirI (d, _, _) = d
distI :: Instr -> Int
distI (_, d, _) = d
colorI :: Instr -> String
colorI (_, _, c) = c

parseInstr :: String -> Instr
parseInstr s = (dir, len, color)
    where
        ss = split isSpace s
        dir = case head ss of
            "D" -> d4 !! 0
            "R" -> d4 !! 1
            "U" -> d4 !! 2
            "L" -> d4 !! 3
        len = read $ ss !! 1
        color = take 6 $ drop 2 $ ss !! 2

shoelace2 :: [Point] -> Int
shoelace2 [p] = 0
shoelace2 ((x1, y1):p@(x2, y2):ps) = x1 * y2 - x2 * y1 + shoelace2 (p:ps)

main :: IO ()
main = do
    input <- getContents
    let instrs = parseInstr <$> parseLines input
    let perim = foldl (\acc i -> acc + distI i) 0 instrs
    let pts = scanl (\acc i -> addPoint acc $ mulPoint (distI i) (dirI i)) (0, 0) instrs
    let area = abs $ shoelace2 pts `div` 2
    print $ area + (perim `div` 2) + 1