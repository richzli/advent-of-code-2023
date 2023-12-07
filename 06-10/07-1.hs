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

type Hand = [(Char, Int)]

value :: Char -> Int
value 'A' = 14
value 'K' = 13
value 'Q' = 12
value 'J' = 11
value 'T' = 10
value x = read [x]

addCard :: Hand -> Char -> Hand
addCard hand card = case hand of
    [] -> [(card, 1)]
    (c, cnt):hs | c == card -> (card, cnt+1):hs
    h:hs -> h:addCard hs card

cardCmp :: (Char, Int) -> (Char, Int) -> Ordering
cardCmp (c1, n1) (c2, n2)
    | n1 /= n2 = compare n1 n2
    | c1 /= c2 = compare (value c1) (value c2)
    | otherwise = EQ

makeHand :: String -> Hand
makeHand s = sortBy (flip cardCmp) $ foldl addCard [] s

score :: Hand -> Int
score h
    | snd (h!!0) == 5 = 7
    | snd (h!!0) == 4 = 6
    | snd (h!!0) == 3 && snd (h!!1) == 2 = 5
    | snd (h!!0) == 3 = 4
    | snd (h!!0) == 2 && snd (h!!1) == 2 = 3
    | snd (h!!0) == 2 = 2
    | otherwise = 1

handCmp :: String -> String -> Ordering
handCmp s1 s2
    | h1 /= h2 = compare h1 h2
    | otherwise = compare (value <$> s1) (value <$> s2)
    where
        h1 = score $ makeHand s1
        h2 = score $ makeHand s2

finalCmp :: (String, Int) -> (String, Int) -> Ordering
finalCmp (h1, b1) (h2, b2) = handCmp h1 h2

parseLine :: String -> (String, Int)
parseLine h = (s!!0, read $ s!!1)
    where s = split isSpace h

main :: IO ()
main = do
    input <- getContents
    let lines = parseLines input
    print $ sum $ zipWith (\a b -> a * snd b) [1..(length lines)] $ sortBy finalCmp $ parseLine <$> lines