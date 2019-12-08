module Main (main) where

import Data.List
import Control.Monad (forM_)

countElem :: Eq a => a -> [a] -> Int
countElem i = length . filter (i==)

chunksOf :: Int -> [e] -> [[e]]
chunksOf i ls = map (take i) (build (splitter ls))
  where splitter :: [e] -> ([e] -> a -> a) -> a -> a
        splitter [] _ n = n
        splitter l c n  = l `c` splitter (drop i l) c n
        build g = g (:) []

parseLayers :: Int -> Int -> String -> [[String]]
parseLayers w h c =  chunksOf h $ filter f $ chunksOf w c
  where f = (w ==) . length

part1 :: [[String]] -> Int
part1 layers = count1 * count2
  where sorted = sortOn (sum . map (countElem '0')) layers
        theLayer = head sorted
        count1 = countElem '1' $ concat theLayer
        count2 = countElem '2' $ concat theLayer

mergeLayers :: [String] -> [String] -> [String]
mergeLayers l1 l2 = (merge <$> l1) `ap` l2
  where mergeLine '0' _ = '0'
        mergeLine '1' _ = '1'
        mergeLine '2' v = v
        merge li1 li2 = (mergeLine <$> li1) `ap` li2
        ap fs a = zipWith (\f x -> f x) fs a

part2 :: Int -> Int -> [[String]] -> [String]
part2 w h layers = foldr mergeLayers (replicate h $ replicate w '2') layers

main :: IO ()
main = do
  c <- readFile "input.txt"
  let layers = parseLayers 25 6 c

  print $ part1 layers
  let res2 = part2 25 6 layers

  let printLine = putStrLn . fmap (\c' -> if c' == '0' then ' ' else '#')
  forM_ res2 printLine
