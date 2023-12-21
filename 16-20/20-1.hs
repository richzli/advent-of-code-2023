import Control.Applicative
import Control.Monad
import Data.Bits
import Data.Char
import Data.Function
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

data Message = Message {
    src, rcv :: String,
    msg :: Bool
}
data Module = FlipFlop | Conjunction | Broadcaster
data State = State {
    flipMem :: Map.Map String Bool,
    conjMem :: Map.Map String (Map.Map String Bool)
}

parseLine :: String -> ((String, Module), (String, [String]))
parseLine s = ((name, mm), (name, strip <$> split (==',') rs))
    where
        [nameRaw, rs] = strip <$> filter (not . null) (split (`elem` "->") s)
        (name, mm) = case head nameRaw of
            '%' -> (tail nameRaw, FlipFlop)
            '&' -> (tail nameRaw, Conjunction)
            _ -> (nameRaw, Broadcaster)

flipState :: String -> State -> State
flipState a state = State (Map.adjust not a $ flipMem state) (conjMem state)

flipCheck :: String -> State -> Bool
flipCheck a state = fromJust $ Map.lookup a $ flipMem state

conjState :: Message -> State -> State
conjState (Message s r m) state = State (flipMem state) (Map.adjust (Map.adjust (const m) s) r $ conjMem state)

conjCheck :: String -> State -> Bool
conjCheck a state = not $ Map.foldr (&&) True $ fromJust $ Map.lookup a $ conjMem state

mul2 :: (Int, Int) -> Int
mul2 (x, y) = x*y

go :: Map.Map String Module -> Map.Map String [String] -> Int
go types outputs = mul2 $ fst $ iterate button ((0, 0), State initFlip initConj) !! 1000
    where
        initFlip = Map.map (const False) types
        initConj = Map.map Map.fromList $ Map.foldrWithKey (\k a m -> foldr (Map.adjust ((k,False):)) m a) (Map.map (const []) types) outputs

        send m a = (\ms -> (if m then (length ms, 0) else (0, length ms), ms)) $ (\r -> Message a r m) <$> fromJust (Map.lookup a outputs)

        -- need to +1 for initial low msg --
        process [] state = ((0, 1), state)
        process (mm@(Message s r m):ms) state = case Map.lookup r types of
            Just FlipFlop -> case m of
                True -> process ms state
                False -> (addPoint msgCount finalCount, finalState)
                    where
                        newState = flipState r state
                        (msgCount, newMsgs) = send (flipCheck r newState) r
                        (finalCount, finalState) = process (ms ++ newMsgs) newState
            Just Conjunction -> (addPoint msgCount finalCount, finalState)
                where
                    newState = conjState mm state
                    (msgCount, newMsgs) = send (conjCheck r newState) r
                    (finalCount, finalState) = process (ms ++ newMsgs) newState
            Just Broadcaster -> (addPoint msgCount finalCount, finalState)
                where
                    (msgCount, newMsgs) = send m r
                    (finalCount, finalState) = process (ms ++ newMsgs) state
            _ -> process ms state
        
        button (oldCount, state) = (addPoint oldCount newCount, newState)
            where
                (newCount, newState) = process [Message "button" "broadcaster" False] state

main :: IO ()
main = do
    input <- getContents
    let rules = unzip $ parseLine <$> parseLines input
    let types = Map.fromList $ fst rules
    let outputs = Map.fromList $ snd rules
    print $ go types outputs