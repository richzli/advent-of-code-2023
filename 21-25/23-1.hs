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

dirs :: String
dirs = "v>^<"

explore :: Map.Map Point Char -> Point -> Map.Map Point Int
explore m p
    | Map.notMember p m = Map.empty
    | otherwise = dfs (Set.singleton p) 1 (addPoint p (fd p))
    where
        fd q = d4 !! fromJust (elemIndex (fromJust $ Map.lookup q m) dirs)
        dfs v d q = case Map.lookup q m of
            Nothing -> Map.singleton q d
            Just '#' -> Map.empty
            Just '.' -> foldl (Map.unionWith max) Map.empty $ dfs (Set.insert q v) (d+1) <$> filter (`Set.notMember` v) (addPoint q <$> d4)
            -- assuming that otherwise it's one of v>^<
            _ | Set.notMember (addPoint q (fd q)) v -> Map.singleton q d
            _ -> Map.empty

getGraph :: Map.Map Point Char -> Map.Map Point (Map.Map Point Int)
getGraph m = go Map.empty [src]
    where
        src = (0, 1)

        go v [] = v
        go v (p:ps) = case Map.lookup p v of
            Nothing -> go (Map.insert p nxt v) (ps ++ Map.keys nxt)
                where
                    nxt = explore m p
            Just _ -> go v ps

solve :: Map.Map Point (Map.Map Point Int) -> Map.Map Point [Point] -> Map.Map Point Int
solve dag rev = dp (Map.singleton src (-1)) (Map.keys $ fromJust $ Map.lookup src dag)
    where
        src = (0, 1)

        dp :: Map.Map Point Int -> [Point] -> Map.Map Point Int
        dp v [] = v
        dp v ps = dp (Map.union v nv) (nub $ concatMap (Map.keys . fromJust . (`Map.lookup` dag)) ok)
            where
                ok = filter (all (`Map.member` v) . fromJust . (`Map.lookup` rev)) ps
                dists = (\q -> maximum $ (\r ->
                            fromJust (Map.lookup r v) + 
                            fromJust (Map.lookup q $ fromJust $ Map.lookup r dag)
                        ) <$> fromJust (Map.lookup q rev)
                    ) <$> ok
                nv = Map.fromList $ zip ok dists

main :: IO ()
main = do
    input <- getContents
    let lines = parseLines input

    let n = length lines
    let m = length $ head lines
    let src = (0, 1)
    let dst = (n, m-2)

    let grid = Map.insert src 'v' $ Map.fromList $ enumerate2d lines
    let dag = Map.insert dst Map.empty $ getGraph grid
    let rev = Map.foldrWithKey (\k v acc -> Map.unionWith (++) (Map.fromList $ (,[k]) <$> Map.keys v) acc) Map.empty dag

    print $ fromJust $ Map.lookup dst $ solve dag rev