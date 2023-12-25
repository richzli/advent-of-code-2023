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

addL = zipWith (+)
subL = zipWith (-)
mulC l c = fmap (*c) l
mulL = zipWith (*)
divL = zipWith (/)

type Hail = ([Rational], [Rational])

parseLine :: String -> Hail
parseLine s = (readCoords x, readCoords v)
    where
        [x, v] = strip <$> split (=='@') s
        readCoords l = fromIntegral . read . strip <$> split (==',') l

-- lmao crud it's not convex...
cvx :: [Hail] -> [[Rational]]
cvx hs = go s3 o3 (fmap (const 0.0) hs) 100000
    where
        step = 0.00001
        ze3 = replicate 3 0
        o3 = replicate 3 1
        s3 = replicate 3 300000000000000

        go as bs ts 0 = [as, bs, ts]
        go as bs ts i = go (as `subL` (da `mulC` step))
                           (bs `subL` (db `mulC` step))
                           (ts `subL` (dt `mulC` step))
                           (i - 1)
            where
                signs = fmap signum <$> zipWith (\(x, v) t -> (as `addL` (bs `mulC` t)) `subL` (x `addL` (v `mulC` t))) hs ts

                da = foldl addL ze3 signs
                db = foldl (\acc (s, t) -> acc `addL` (s `mulC` t)) ze3 $ zip signs ts
                dt = zipWith (\s d -> sum $ s `mulL` d) signs $ (\(_, v) -> bs `subL` v) <$> hs

---

check :: [Rational] -> Hail -> Hail -> Maybe [Rational]
check v (x1, v1) (x2, v2) = r
    where
        (xx1@[px, py], [z1]) = splitAt 2 x1
        (xx2, [z2]) = splitAt 2 x2
        (vv, [vz]) = splitAt 2 v
        (vv1@[a, c], [vz1]) = splitAt 2 $ v1 `subL` v
        (vv2@[b, d], [vz2]) = splitAt 2 $ v `subL` v2

        pos px pv t = px + pv * t

        det = a * d - b * c
        inv = case det of
            0 -> Nothing
            _ -> Just $ (`mulC` (1/det)) <$> [[d, -b], [-c, a]]
        t = fmap (sum . (`mulL` (xx2 `subL` xx1))) <$> inv

        r = case (t, nub . (`addL` [z1, z2]) . (`mulL` [vz1, -vz2]) <$> t) of
            (Just [t1, t2], Just [pz]) -> Just [pos px a t1, pos py c t1, pz]
            _ -> Nothing

data Intersection = Nowhere | Time Rational | Everywhere deriving (Show, Eq)

combineInter :: Intersection -> Intersection -> Intersection
combineInter i1 i2 = case (i1, i2) of
    (Nowhere, _) -> Nowhere
    (_, Nowhere) -> Nowhere
    (x, Everywhere) -> x
    (Everywhere, x) -> x
    (Time t1, Time t2) | t1 == t2 -> i1
    _ -> Nowhere

line :: Rational -> Rational -> Rational -> Intersection
line m b y = case m of
    0 -> if b == y then Everywhere else Nowhere
    _ -> Time ((y-b)/m)

foldLine :: [Rational] -> [Rational] -> [Rational] -> Intersection
foldLine [] [] [] = Everywhere
foldLine (m:ms) (b:bs) (y:ys) = combineInter (line m b y) (foldLine ms bs ys)

inter :: [Rational] -> [Rational] -> Hail -> Intersection
inter v p (hx, hv) = foldLine vv hx p
    where
        vv = hv `subL` v

solve :: [Hail] -> [Rational]
solve (h1:h2:hs) = head $ mapMaybe ok l
    where
        cube x = xx ++ yy ++ zz
            where
                zz = (:) <$> [-x..x] <*>
                    ((:) <$> [-x..x] <*>
                    (singleton <$> [-x, x]))
                yy = (:) <$> [-x..x] <*>
                    ((:) <$> [-x, x] <*>
                    (singleton <$> [-x+1..x-1]))
                xx = (:) <$> [-x, x] <*>
                    ((:) <$> [-x+1..x-1] <*>
                    (singleton <$> [-x+1..x-1]))
        l = [0, 0, 0] : concatMap cube [1..]

        ok v = case p of
            Nothing -> Nothing
            Just pp -> if all (/= Nowhere) (inter v pp <$> hs) then Just pp else Nothing
                where
                    fromTime i = case i of
                        Time x -> x
            where
                p = check v h1 h2

main :: IO ()
main = do
    input <- getContents
    let hails = parseLine <$> parseLines input
    print $ round <$> solve (take 6 hails)