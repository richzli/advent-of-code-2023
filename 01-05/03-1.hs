import Control.Applicative
import Control.Monad
import Data.Char
import Data.List
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

--- utility ---

enumerate :: [a] -> [(Int, a)]
enumerate = zip [0..]

enumerate2d :: [[a]] -> [(Point, a)]
enumerate2d grid = [((x, y), c)
    | (x, row) <- enumerate grid
    , (y, c) <- enumerate row
    ]

---

isPart :: Char -> Bool
isPart c = not (isDigit c) && (c /= '.')

getParts :: [[Char]] -> [Point]
getParts grid = [ p | (p, c) <- enumerate2d grid, isPart c ]

getNeighbors :: [Point] -> Set.Set Point
getNeighbors ps = foldl Set.union Set.empty $ Set.fromList . neighbors d8 <$> ps

nextDigit :: Maybe Int -> Int -> Maybe Int
nextDigit x d = case x of
    Just x -> Just (10 * x + d)
    Nothing -> Nothing

parseRow :: Set.Set Point -> [(Point, Char)] -> ((Maybe Int, Bool), [(Maybe Int, Bool)])
parseRow nbrs row
    | null row = ((Nothing, False), [])
    | isNothing x && isDigit c = ((Just (read [c]), Set.member p nbrs), xs)
    | isJust x && isDigit c = ((nextDigit x $ read [c], b || Set.member p nbrs), xs)
    | not $ isDigit c = ((Nothing, Set.member p nbrs), (x, b) : xs)
    where
        (p, c) = head row
        ((x, b), xs) = parseRow nbrs $ tail row

fixRow :: Int -> [(Int, a)] -> [(Point, a)]
fixRow r l = reverse [ ((r, y), c)
    | (y, c) <- l
    ]

parseGrid :: [[Char]] -> [Maybe Int]
parseGrid grid =
    [ x
    | ((xx, bb), xs) <- parseRow ps . uncurry fixRow <$> enumerate (enumerate <$> grid)
    , (x, b) <- (xx, bb) : xs
    , isJust x, b
    ]
    where
        ps = getNeighbors $ getParts grid

main :: IO ()
main = do
    input <- getContents
    let grid = parseLines input
    print $ sum . catMaybes $ parseGrid grid