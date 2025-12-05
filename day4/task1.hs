letters :: String -> [Int]
letters n = [if d == '@' then 1 else 0 | d <- n]

addlines :: [Char] -> Int -> [Char]
addlines [] _  = []
addlines str w = take w str ++ "\n" ++ addlines (drop w str) w

neighbours :: Int -> Int -> Int -> [Int]
neighbours i w sz = filter valid offsets
  where
    r = i `div` w
    c = i `mod` w
    offsets = [ i - w - 1
              , i - w
              , i - w + 1
              , i - 1
              , i + 1
              , i + w - 1
              , i + w
              , i + w + 1
              ]
    valid p = p >= 0 && p < sz &&
              let pr = p `div` w
                  pc = p `mod` w
              in abs (pr - r) <= 1 && abs (pc - c) <= 1

countneighbours :: [Int] -> Int -> [Int]
countneighbours flat w = [sum [flat !! j | j <- neighbours i w sz] | i <- [0..sz-1]]
  where
    sz = length flat

calculate :: [Int] -> Int -> [Bool]
calculate flat w = [counts !! i < 4 && alpha == 1 | (i, alpha) <- zip [0..] flat]
  where
    counts = countneighbours flat w

main = do
    input <- readFile "input.txt"
    let matrix = [letters l | l <- lines input]
        width = length $ head matrix
        flat = concat matrix
        forklift_certified = calculate flat width
        result = [if f then 'x' else if o == 1 then '@' else '.' | (o, f) <- zip flat forklift_certified]
        output = addlines result width
        count = length [True | f <- forklift_certified, f]
    --putStrLn output
    print count
