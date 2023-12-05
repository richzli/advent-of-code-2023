import System.IO

split :: (a -> Bool) -> [a] -> [[a]]
split p s = case break p s of
    ([], []) -> []
    (a, b) -> a : split p (drop 1 b)

parseGroups :: String -> [[String]]
parseGroups s = split null $ split (=='\n') s

-- 2022 day 1, for a quick warmup
main :: IO ()
main = do
    input <- getContents
    print $ maximum $ sum <$> (fmap (read::String->Int) <$> parseGroups input)