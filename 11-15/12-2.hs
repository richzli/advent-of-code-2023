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

replaceUnknowns :: String -> [String]
replaceUnknowns [] = [""]
replaceUnknowns (c:cs) = case c of
    '?' -> [('#':), ('.':)] <*> replaceUnknowns cs
    _ -> [(c:)] <*> replaceUnknowns cs

check :: [Int] -> String -> Bool
check is s = is == (length <$> filter (not . null) (split (=='.') s))

checkLine :: String -> Int
checkLine l = length $ filter (check is) $ replaceUnknowns s
    where
        k = split isSpace l
        s = head k
        is = read <$> (split (==',') $ head $ tail k)

canReplace :: String -> Int -> Maybe String
canReplace "" 0 = Just ""
canReplace (c:cs) 0 = case c of
    '#' -> Nothing
    _ -> Just cs
canReplace "" _ = Nothing
canReplace (c:cs) i = case c of
    '.' -> Nothing
    _ -> canReplace cs (i-1)

parseLine :: String -> (String, [Int])
parseLine l = (s, is)
    where
        k = split isSpace l
        s = intercalate "?" $ replicate 5 $ head k
        is = concat $ replicate 5 $ read <$> (split (==',') $ head $ tail k)

type M = Map.Map (String, [Int]) Int

memoize :: (String, [Int]) -> Int -> (M, Int) -> (M, Int)
memoize state x (m, x2) = (Map.insert state (x+x2) m, x+x2)

dp :: M -> (String, [Int]) -> (M, Int)
dp cache ("", []) = (cache, 1)
dp cache ("", _) = (cache, 0)
dp cache (s, []) = if all (/='#') s then (cache, 1) else (cache, 0)
dp cache state@(c:cs, i:is) = case Map.lookup state cache of
    Just x -> memoize state 0 (cache, x)
    Nothing -> case (c, canReplace (c:cs) i) of
        ('.', _) -> memoize state 0 (cache2, x2)
        ('#', Nothing) -> memoize state 0 (cache, 0)
        ('#', Just s2) -> memoize state 0 $ dp cache (s2, is)
        (_, Nothing) -> memoize state 0 (cache2, x2)
        (_, Just s2) -> memoize state x2 $ dp cache2 (s2, is)
        where
            (cache2, x2) = dp cache (cs, i:is)

main :: IO ()
main = do
    input <- getContents
    print $ sum $ snd . dp Map.empty . parseLine <$> parseLines input 