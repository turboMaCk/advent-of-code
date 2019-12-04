module Day1 where

-- Part1

input :: String -> IO [Int]
input name = do
   (fmap readInt . lines) <$> readFile name

fuelReq :: Int -> Int
fuelReq f = floor ((fromIntegral f) / 3) - 2

readInt :: String -> Int
readInt = read

part1 :: IO Int
part1 = do
  c <- input "input.txt"
  pure $ sum $ fuelReq <$> c

-- Part2

totalFuel :: Int -> Int
totalFuel f =
  let res = fuelReq f
  in
  if res <= 0 then 0 else res + totalFuel res

part2 :: IO Int
part2 = do
  c <- input "input.txt"
  pure $ sum $ totalFuel <$> c


main :: IO ()
main = do
  part1 >>= print
  part2 >>= print
