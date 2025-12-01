parse :: String -> Int
parse (s:n) = if s == 'L' then -(read n) else read n

move :: Int -> Int -> Int
move n delta = (n + delta) `mod` 100

zeros :: Int -> Int -> Int
zeros prev delta = circles + if special_case then 1 else 0
  where
    circles = abs delta `div` 100
    r = abs delta `mod` 100
    tmp = prev + signum delta * r
    pos = tmp `mod` 100
    crosses = tmp >= 100 || tmp <= 0
    moves = pos /= prev
    special_case = crosses && moves && prev /= 0

main :: IO ()
main = do
    input <- readFile "input.txt"
    let deltas = map parse (lines input)
        positions = scanl move 50 deltas
        moving = sum [zeros p d | (p, d) <- zip positions deltas]

    print moving
