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

digits :: Int -> Int
digits n = floor (logBase 10 (fromIntegral n)) + 1

part :: Int -> Int -> Int -> Int
part n index length = (n `mod` top) `div` bottom
  where
    d = digits n
    top = 10^(d - index)
    bottom = 10^(d - index - length)

hasRepeatingPattern :: Int -> Int -> Bool
hasRepeatingPattern n patternLen = 
    d `mod` patternLen == 0 && allEqual && firstPart > 0
  where
    d = digits n
    repetitions = d `div` patternLen
    parts = [part n (i * patternLen) patternLen | i <- [0..repetitions-1]]
    firstPart = head parts
    allEqual = all (== firstPart) parts

isInvalid :: Int -> Bool
isInvalid n = any (hasRepeatingPattern n) [1..d `div` 2]
  where
    d = digits n

invalids :: (Int, Int) -> Int
invalids (start, end) = sum [n | n <- [start..end], isInvalid n]

main :: IO ()
main = do
    input <- readFile "input.txt"
    let intervals = parse input
        result = sum [invalids interval | interval <- intervals]
    print result