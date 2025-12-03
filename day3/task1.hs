digits :: Integer -> [(Integer, Integer)]
digits n = zip [0..] [read [d] | d <- show n]

joltage :: Integer -> Integer
joltage bank = maximum [b1 * 10 + b2 | (i1, b1) <- digits bank, (i2, b2) <- digits bank, i1 < i2]

-- bruteforce, change 12 with 2 in task2.hs for fast execution
main :: IO ()
main = do
    input <- readFile "input.txt"
    let banks = map read (lines input)
        result = sum [joltage b | b <- banks]
    print result
