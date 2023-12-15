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

hash :: String -> Int
hash "" = 0
hash (c:cs) = ((hash cs + ord c) * 17) `mod` 256

type Lens = (String, Int)

remove :: String -> [Lens] -> [Lens]
remove s [] = []
remove s (l@(label, _):ls) = case label of
    t | t == s -> ls
    _ -> l:remove s ls

replace :: Lens -> [Lens] -> [Lens]
replace l [] = [l]
replace l@(label, focal) (l2@(label2, _):ls) = case label2 of
    t | t == label -> l:ls
    _ -> l2:replace l ls

data Operation = Remove String | Replace Lens

parseOperation :: String -> Operation
parseOperation o
    | '=' `elem` o = Replace (x!!0, read (x!!1))
    | '-' `elem` o = Remove (init o)
    where
        x = split (=='=') o

box :: Operation -> Int
box op = case op of
    Replace (s, _) -> hash $ reverse s
    Remove s -> hash $ reverse s

operateBox :: Operation -> Maybe [Lens] -> [Lens]
operateBox op l = case (l, op) of
    (Nothing, Remove _) -> []
    (Nothing, Replace l) -> [l]
    (Just x, Remove s) -> remove s x
    (Just x, Replace l) -> replace l x

operate :: Map.Map Int [Lens] -> String -> Map.Map Int [Lens]
operate m o = Map.insert ind (operateBox op $ Map.lookup ind m) m
    where
        op = parseOperation o
        ind = box op

score :: Map.Map Int [Lens] -> Int
score m = Map.foldrWithKey (\k a b -> b + (k + 1) * (sum $ zipWith (\x y -> x * snd y) [1..] a)) 0 m

main :: IO ()
main = do
    input <- getContents
    let steps = split (==',') $ filter (/='\n') input
    print $ score $ foldl operate Map.empty steps