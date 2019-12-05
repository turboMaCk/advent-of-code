module Main (main) where

import Data.Maybe

-- PART1

range :: [Int]
range = [153517..630395]

v1 :: Int -> Maybe String
v1 int = validate 'x' chars
  where chars = show int
        validate _ [] = Nothing
        validate prev (h:t) =
          if prev == h then
            Just chars
          else
            validate h t

v2 :: String -> Maybe String
v2 chars = validate '0' "" chars
  where validate _ _ [] = Just $ reverse chars
        validate prev acc (h:t) =
          if prev > h then
            Nothing
          else
            validate h (h:acc) t


part1 :: Int
part1 = length $ mapMaybe validate range
  where
    validate x = v1 x >>= v2

-- PART2

v3 :: String -> Maybe String
v3 chars = validate chars
  where validate [] = Nothing
        validate (h:t) =
          if length ((h ==) `filter` chars) == 2 then
            Just chars
          else
            validate t

part2 :: Int
part2 = length $ mapMaybe (validate . show) range
  where validate x = v2 x >>= v3

main :: IO ()
main = do
  print part1
  print part2
