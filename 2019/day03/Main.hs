module Main (main) where

import Data.List

-- Part1

-- (x, y)
type Pos = (Int, Int)

parsePath :: String -> [String]
parsePath str = reverse $ fst $ foldl step' ([], "") str
  where
    step' (res, s) ','  = (s : res, "")
    step' (res, s) v    = (res, s ++ [v])

-- reversed order
step :: String -> Pos -> ([ Pos ], Pos)
step (p:count) (x, y) =
  case p of
    'R' -> ((\x' -> (x', y)) <$> reverse [x+1..x+c], (x+c, y))
    'L' -> ((\x' -> (x', y)) <$> [x-c..x-1], (x-c, y))
    'U' -> ((\y' -> (x, y')) <$> reverse [y+1..y+c], (x, y+c))
    'D' -> ((\y' -> (x, y')) <$> [y-c..y-1], (x, y-c))
  where
    c :: Int
    c = read count

toPositions :: [String] -> ([Pos], Pos) -> [Pos]
toPositions [] (res, _) = reverse res
toPositions (h:t) (acc, pos) =
  toPositions t (newSteps ++ acc, newPos)
  where
    (newSteps, newPos) = step h pos

part1 :: IO Int
part1 = do
  [line1, line2] <- lines <$> readFile "input.txt"
  let pos1 = toPositions (parsePath line1) ([], (0,0))
  let pos2 = toPositions (parsePath line2) ([], (0,0))

  putStrLn "positions:"
  print $ take 10 pos1
  print $ take 10 pos2

  -- God help us with this complexity
  let crosses = filter (flip elem pos2) pos1

  putStrLn "crosses:"
  print $ take 10 crosses
  let distances = (\(x, y) -> abs x + abs y) <$> crosses
  putStrLn "distances:"
  print $ take 10 distances
  putStrLn "result:"
  pure $ head $ sort distances

-- Part2

mapMaybei :: (Int -> a -> Maybe b) -> [a] -> [b]
mapMaybei f = mapMaybei' f 0 []

mapMaybei' :: (Int -> a -> Maybe b) -> Int -> [b] -> [a] -> [b]
mapMaybei' _ _ acc [] = acc
mapMaybei' f i acc (h:t) = mapMaybei' f (i+1) newAcc t
  where newAcc =
          case f i h of
            Just b -> b:acc
            Nothing -> acc

part2 :: IO Int
part2 = do
  [line1, line2] <- lines <$> readFile "input.txt"
  let pos1 = toPositions (parsePath line1) ([], (0,0))
  let pos2 = toPositions (parsePath line2) ([], (0,0))

  putStrLn "positions:"
  print $ take 10 pos1
  print $ take 10 pos2

  -- God help us with this complexity
  let crossesDist = mapMaybei (\i x -> (i +) <$> (x `elemIndex` pos2)) pos1

  putStrLn "crossesDist:"
  print $ take 10 crossesDist

  putStrLn "result:"
  pure $ 2 + (head $ sort crossesDist) -- need to add 1 extra step for each path

main :: IO ()
main = do
  part1 >>= print
  part2 >>= print
