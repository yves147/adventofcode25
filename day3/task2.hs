greedy :: Ord a => Int -> [a] -> [a]
greedy 0 _ = []
greedy _ [] = []
greedy n xs = 
  let remaining = n - 1
      bdelta = take (length xs - remaining) xs -- search space
      delta = maximum bdelta
      tau = length (takeWhile (/= delta) bdelta) -- index of delta
  in delta : greedy (n - 1) (drop (tau + 1) xs)

joltage :: String -> Integer
joltage line = read (greedy 12 line)

main :: IO ()
main = do
    input <- readFile "input.txt"
    let result = sum [joltage b | b <- lines input]
    print result
