import Control.Monad
import Data.Char
import Data.List
import Data.Maybe
import System.IO

split :: (a -> Bool) -> [a] -> [[a]]
split p s = case break p s of
    (a, []) -> [a]
    (a, b) -> a : split p (drop 1 b)

strip :: String -> String
strip s = dropWhile isSpace $ dropWhileEnd isSpace s

parseLines :: String -> [String]
parseLines = dropWhileEnd null . split (=='\n')

parseGroups :: String -> [[String]]
parseGroups = split null . parseLines

---

data Cubes = Cubes {
    red :: Int,
    blue :: Int,
    green :: Int
} deriving (Eq, Show)

emptyCubes :: Cubes
emptyCubes = Cubes 0 0 0
plusC :: Cubes -> Cubes -> Cubes
(Cubes r1 b1 g1) `plusC` (Cubes r2 b2 g2) = Cubes {
    red = r1 + r2,
    blue = b1 + b2,
    green = g1 + g2
}
maxC :: Cubes -> Cubes -> Cubes
(Cubes r1 b1 g1) `maxC` (Cubes r2 b2 g2) = Cubes {
    red = max r1 r2,
    blue = max b1 b2,
    green = max g1 g2
}

redStr, blueStr, greenStr :: String
redStr = "red"
blueStr = "blue"
greenStr = "green"

data Game = Game {
    gid :: Int,
    rounds :: [Cubes]
}

parseSingleCube :: String -> Cubes
parseSingleCube s
    | c == redStr = Cubes x 0 0
    | c == blueStr = Cubes 0 x 0
    | c == greenStr = Cubes 0 0 x
    | otherwise = Cubes 0 0 0
    where
        l = split (==' ') s
        x = read $ head l
        c = head $ tail l

parseCubes :: String -> Cubes
parseCubes s = foldl plusC emptyCubes $ parseSingleCube . strip <$> split (==',') s

parseGame :: String -> Game
parseGame s =
    Game gid (parseCubes <$> turns)
    where
        g = strip <$> split (==':') s
        gid = read $ split (==' ') (head g) !! 1
        turns = strip <$> split (==';') (g !! 1)

getMax :: [Cubes] -> Cubes
getMax = foldl maxC emptyCubes

okC :: Cubes -> Bool
okC c = (red c <= 12) && (green c <= 13) && (blue c <= 14)

power :: Cubes -> Int
power c = red c * blue c * green c

---

main :: IO ()
main = do
    input <- getContents
    let games = parseGame <$> parseLines input
    -- print (sum $ (\g -> if okC (getMax $ rounds g) then gid g else 0) <$> games)
    print (sum $ power . getMax . rounds <$> games)