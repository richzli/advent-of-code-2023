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

roll :: String -> String
roll s = intercalate "#" $ sort <$> split (=='#') s

score :: [String] -> Int
score g = sum $ sum . zipWith (\x y -> if y == 'O' then x else 0) [1..] <$> g

rotate :: [String] -> [String]
rotate g = reverse <$> transpose g

type State = Map.Map ([String], Int) Int

solve :: State -> Int -> ([String], Int) -> Int
solve m 0 state@(grid, dir) = score $ iterate Main.rotate grid !! dir
solve m steps state@(grid, dir) = case Map.lookup state m of
    Just prev -> solve Map.empty ((steps `mod` (prev - steps)) - 1) (grid2, dir2)
    Nothing -> solve (Map.insert state steps m) (steps - 1) (grid2, dir2)
    where
        grid2 = Main.rotate $ roll <$> grid
        dir2 = (dir + 3) `mod` 4

main :: IO ()
main = do
    input <- getContents
    let panels = fmap reverse . transpose <$> parseGroups input
    print $ sum $ solve Map.empty (4 * 1000000000) . (,0) <$> panels