parse :: String -> Int
parse (s:n) = if s == 'L' then -(read n) else read n

move :: Int -> Int -> Int
move eta lambda = (eta + lambda) `mod` 100

zeros :: Int -> Int -> Int
zeros prev lambda = circles + if special_case then 1 else 0
  where
    circles = abs lambda `div` 100
    r = abs lambda `mod` 100
    tmp = prev + signum lambda * r
    pos = tmp `mod` 100
    crosses = tmp >= 100 || tmp <= 0
    moves = pos /= prev
    special_case = crosses && moves && prev /= 0

main :: IO ()
main = do
    input <- readFile "input.txt"
    let Lambda = map parse (lines input)
        positions = scanl move 50 Lambda
        moving = sum [zeros p d | (p, d) <- zip positions Lambda]

    print moving
