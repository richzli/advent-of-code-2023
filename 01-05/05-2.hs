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

data Range = Range {
    dst :: Int,
    src :: Int,
    len :: Int
} deriving (Show)

parseRange :: String -> Range
parseRange s = Range (ns!!0) (ns!!1) (ns!!2) where ns = parseNumbers s

inRange :: Range -> Int -> Bool
inRange r i = i >= src r && i < src r + len r

applyRange :: Range -> Int -> Int
applyRange r i = if inRange r i then dst r - src r + i else i

applyMap :: [Range] -> Int -> Int
applyMap rs i = case rs of
    [] -> i
    t:ts -> if inRange t i then applyRange t i else applyMap ts i

---

data Seeds = Seeds {
    bgn :: Int,
    end :: Int
} deriving (Show)

parseSeeds :: [Int] -> [Seeds]
parseSeeds l = case l of
    [] -> []
    s:n:xs -> Seeds s (s+n-1) : parseSeeds xs

isolate :: Range -> Maybe Seeds -> (Maybe Seeds, [Maybe Seeds])
isolate r Nothing = (Nothing, [])
isolate r (Just s)
    | end s < src r || bgn s >= src r + len r = (Nothing, [Just s])
    | bgn s >= src r && end s < src r + len r = (Just s, [])
    | bgn s < src r && end s >= src r && end s < src r + len r =
        (Just (Seeds (src r) (end s)), [Just (Seeds (bgn s) (src r - 1))])
    | bgn s >= src r && bgn s < src r + len r && end s >= src r + len r =
        (Just (Seeds (bgn s) (src r + len r - 1)), [Just (Seeds (src r + len r) (end s))])
    | bgn s < src r && end s >= src r + len r =
        (Just (Seeds (src r) (src r + len r - 1)), [Just (Seeds (bgn s) (src r - 1)), Just (Seeds (src r + len r) (end s))])
    | otherwise = (Nothing, [])

applyMapSeeds :: [Range] -> Maybe Seeds -> [Maybe Seeds]
applyMapSeeds l s = case l of
    [] -> [s]
    r:rs -> case isolate r s of
        (Nothing, rem) -> concatMap (applyMapSeeds rs) rem
        (Just sd, rem) -> Just (Seeds (applyRange r (bgn sd)) (applyRange r (end sd))) : concatMap (applyMapSeeds rs) rem

main :: IO ()
main = do
    input <- getContents
    let groups = parseGroups input
    let seeds = parseSeeds $ parseNumbers (split (==':') (head $ head groups) !! 1)
    let maps = fmap parseRange . tail <$> tail groups
    -- print $ minimum $ (\s -> foldl (flip applyMap) s maps) <$> seeds
    -- print $ minimum . catMaybes $ fmap bgn <$> foldl (\s m -> concatMap (applyMapSeeds m) s) (Just <$> seeds) maps
    print $ minimum $ mapMaybe (fmap bgn) $ foldl (\s m -> concatMap (applyMapSeeds m) s) (Just <$> seeds) maps
