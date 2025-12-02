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

isInvalid :: Int -> Bool
isInvalid n = even d && firstHalf > 0 && firstHalf == secondHalf
  where
    d = digits n
    halfLength = d `div` 2
    firstHalf = part n 0 halfLength
    secondHalf = part n halfLength halfLength

invalids :: (Int, Int) -> Int
invalids (start, end) = sum [n | n <- [start..end], isInvalid n]

main :: IO ()
main = do
    input <- readFile "example.txt"
    let intervals = parse input
        result = sum [invalids interval | interval <- intervals]
    print result