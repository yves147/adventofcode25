import Data.List (unfoldr)

-- https://lotz84.github.io/haskellbyexample/ex/string-functions
split :: String -> Char -> [String]
split "" _ = []
split xs c = let (ys, zs) = break (== c) xs
             in  if null zs then [ys] else ys : split (tail zs) c

toInterval :: String -> (Int, Int)
toInterval input = (read start, read end)
  where [start, end] = split input '-'

parse :: String -> [(Int, Int)]
parse input = map toInterval (split input ',')

hasRepeatingPattern :: String -> Int -> Bool
hasRepeatingPattern s patternLen = 
    length s `mod` patternLen == 0 && 
    head pattern /= '0' &&
    all (== pattern) chunks
  where
    pattern = take patternLen s
    chunks = takeWhile (not . null) $ unfoldr (\str -> 
        if null str then Nothing 
        else Just (splitAt patternLen str)) s

isInvalid :: Int -> Bool
isInvalid n = any (hasRepeatingPattern s) [1..length s `div` 2]
  where
    s = show n

invalids :: (Int, Int) -> Int
invalids (start, end) = sum [n | n <- [start..end], isInvalid n]

main :: IO ()
main = do
    input <- readFile "input.txt"
    let intervals = parse input
        result = sum [invalids interval | interval <- intervals]
    print result