parse :: String -> Int
parse (s:n) = if s == 'L' then -(read n) else read n

move :: Int -> Int -> Int
move n delta = (n + delta) `mod` 100

main :: IO ()
main = do
    input <- readFile "input.txt"
    let deltas = map parse (lines input)
        positions = scanl move 50 deltas
        result = length $ filter (==0) positions
    print result
