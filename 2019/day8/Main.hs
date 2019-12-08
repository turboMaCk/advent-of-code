module Main (main) where

import Data.List

countElem :: Eq a => a -> [a] -> Int
countElem i = length . filter (i==)

chunksOf :: Int -> [e] -> [[e]]
chunksOf i ls = map (take i) (build (splitter ls))
  where
    splitter :: [e] -> ([e] -> a -> a) -> a -> a
    splitter [] _ n = n
    splitter l c n  = l `c` splitter (drop i l) c n
    build g = g (:) []

indexed :: [a] -> [ (Int, a) ]
indexed = reverse . snd . foldl go (0, [])
  where go (i, acc) a = (i+1, (i, a):acc)

parseLayers :: Int -> Int -> String -> [(Int, [String])]
parseLayers w h c = indexed $ chunksOf h $ filter f $ chunksOf w c
  where f = (w ==) . length

part1 :: [(Int, [String])] -> Int
part1 layers = count1 * count2
  where sorted = sortOn (sum . map (countElem '0') . snd) layers
        theLayer = snd $ head sorted
        count1 = countElem '1' $ concat theLayer
        count2 = countElem '2' $ concat theLayer

main :: IO ()
main = do
  c <- readFile "input.txt"
  let layers = parseLayers 25 6 c

  print $ part1 layers
