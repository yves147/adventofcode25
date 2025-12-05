import           Control.Monad               (forM_, when)
import           Control.Monad.ST            (runST)
import qualified Data.Vector.Unboxed         as V
import qualified Data.Vector.Unboxed.Mutable as MV

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

countneighbours :: V.Vector Int -> Int -> V.Vector Int
countneighbours grid w = runST $ do
    let sz = V.length grid
    counts <- MV.replicate sz 0

    forM_ [0..sz-1] $ \i -> do
        when (grid V.! i == 1) $ do
            let nbs = neighbours i w sz
            forM_ nbs $ \j -> do
                !alpha <- MV.read counts j
                MV.write counts j (alpha + 1)

    V.unsafeFreeze counts

accessible :: V.Vector Int -> Int -> V.Vector Bool
accessible grid w = V.generate sz check
  where
    sz = V.length grid
    counts = countneighbours grid w
    check i = (grid V.! i == 1) && (counts V.! i < 4)

remove :: V.Vector Int -> Int -> (V.Vector Int, Int)
remove grid w = (newgrid, removed)
  where
    acc = accessible grid w
    newgrid = V.imap (\i alpha -> if acc V.! i then 0 else alpha) grid
    removed = V.length $ V.filter id acc

removeall :: V.Vector Int -> Int -> (Int, [V.Vector Int])
removeall grid0 w = go grid0 0 [grid0]
  where
    go grid total grids =
      let (newgrid, count) = remove grid w
      in if count == 0
         then (total, reverse grids)
         else go newgrid (total + count) (newgrid : grids)

visualize :: V.Vector Int -> Int -> String
visualize grid w = addlines vis w
  where
    sz = V.length grid
    acc = accessible grid w
    vis = V.toList $ V.generate sz $ \i ->
        if acc V.! i
        then 'x'
        else if grid V.! i == 1 then '@' else '.'

main :: IO ()
main = do
    input <- readFile "input.txt"
    let matrix = [letters l | l <- lines input]
        width = length $ head matrix
        flat = V.fromList $ concat matrix
        (count, grids) = removeall flat width
    --forM_ (zip [0..] grids) $ \(step, g) -> putStrLn $ visualize g width
    print count
