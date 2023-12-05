import Control.Monad
import Data.Char
import Data.List
import Data.Maybe
import System.IO

split :: (a -> Bool) -> [a] -> [[a]]
split p s = case break p s of
    (a, []) -> [a]
    (a, b) -> a : split p (drop 1 b)

parseLines :: String -> [String]
parseLines = split (=='\n')

parseGroups :: String -> [[String]]
parseGroups = split null . parseLines

main :: IO ()
main = do
    input <- getContents
    print ""