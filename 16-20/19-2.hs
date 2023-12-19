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

data State = Accept | Reject | At String deriving Eq
type Part = Map.Map String Int
data Rule = Rule {
    cond :: Part -> Bool,
    next :: State
}

parseState :: String -> State
parseState s = case s of
    "A" -> Accept
    "R" -> Reject
    _ -> At s

parseCond :: String -> (Part -> Bool)
parseCond str = rel (read num) . fromJust . Map.lookup var
    where
        rel = if '>' `elem` str then (<) else (>)
        [var, num] = split (`elem` "<>") str

parseRule :: String -> Rule
parseRule str = case split (==':') str of
    [end] -> Rule (const True) (parseState end)
    [cond, end] -> Rule (parseCond cond) (parseState end)

parseWorkflow :: String -> (String, [Rule])
parseWorkflow str = (name, parseRule <$> split (==',') rules)
    where
        name:rules:_ = split (`elem` "{}") str

to2Tuple :: [String] -> (String, Int)
to2Tuple [var, num] = (var, read num)

parsePart :: String -> Part
parsePart str = Map.fromList $ to2Tuple . split (=='=') <$> split (==',') (init $ drop 1 str)

evaluate :: [Rule] -> Part -> State
evaluate (r:rs) p = if (cond r) p then (next r) else (evaluate rs p)

go :: Map.Map String [Rule] -> State -> Part -> State
go wfs s p = case s of
    At w -> go wfs (evaluate (fromJust $ Map.lookup w wfs) p) p
    _ -> s

---

type PartRange = Map.Map String (Int, Int)
data RuleRange = RuleRange {
    condR :: PartRange -> (Maybe PartRange, Maybe PartRange),
    nextR :: State
}

parseCondRange :: String -> PartRange -> (Maybe PartRange, Maybe PartRange)
parseCondRange str p = case (n `rel` l, n `rel` r) of
    (True, True) -> (Just p, Nothing)
    (False, False) -> (Nothing, Just p)
    (False, True) -> (Just (Map.insert var (n+1, r) p), Just (Map.insert var (l, n) p))
    (True, False) -> (Just (Map.insert var (l, n-1) p), Just (Map.insert var (n, r) p))
    where
        rel = if '>' `elem` str then (<) else (>)
        [var, num] = split (`elem` "<>") str
        n = read num
        l = fst $ fromJust $ Map.lookup var p
        r = snd $ fromJust $ Map.lookup var p

parseRuleRange :: String -> RuleRange
parseRuleRange str = case split (==':') str of
    [end] -> RuleRange (\p -> (Just p, Nothing)) (parseState end)
    [cond, end] -> RuleRange (parseCondRange cond) (parseState end)

parseWorkflowRange :: String -> (String, [RuleRange])
parseWorkflowRange str = (name, parseRuleRange <$> split (==',') rules)
    where
        name:rules:_ = split (`elem` "{}") str

evaluateRange :: [RuleRange] -> Maybe PartRange -> [(State, PartRange)]
evaluateRange _ Nothing = []
evaluateRange (r:rs) (Just p) = case (condR r) p of
    (Nothing, xx) -> evaluateRange rs xx
    (Just p2, xx) -> (nextR r, p2):evaluateRange rs xx

goRange :: Map.Map String [RuleRange] -> (State, PartRange) -> [PartRange]
goRange wfs (s, p) = case s of
    At w -> concatMap (goRange wfs) (evaluateRange (fromJust $ Map.lookup w wfs) (Just p))
    Accept -> [p]
    Reject -> []

combs :: PartRange -> Int
combs = Map.foldr (\(l, r) acc -> acc * (r - l + 1)) 1

main :: IO ()
main = do
    input <- getContents
    let [wfsRaw, partsRaw] = parseGroups input
    let wfs = Map.fromList $ parseWorkflowRange <$> wfsRaw
    print $ sum $ combs <$> goRange wfs (At "in", Map.fromList [("x", (1, 4000)),("m", (1, 4000)),("a", (1, 4000)),("s", (1, 4000))])