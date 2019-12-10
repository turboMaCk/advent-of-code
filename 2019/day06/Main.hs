module Main (main) where

import Data.Maybe

-- Part 1

parse :: String -> (String, String)
parse rel = parse' (rel, "")
  where
    parse' (')':rest, acc) = (rest, reverse $ acc)
    parse' (h:rest, acc) = parse' (rest, h:acc)


getDirectOrbitsFor :: String -> [(String, String)] -> [String]
getDirectOrbitsFor id' = mapMaybe (\(x, orb) -> if x == id' then Just orb else Nothing)

getOrbitsFor :: String -> [(String, String)] -> [String]
getOrbitsFor id' orbits =
  case getDirectOrbitsFor id' orbits of
    [] -> []
    res ->
      res ++ (concat $ (flip getOrbitsFor orbits) <$> res)

part1 :: [(String, String)] -> Int
part1 orbits =
  -- OMG no... my decision to not to use anything but base hunts me
  -- I would really like to have data structure with constant/log time lookups
  sum $ (length . flip getOrbitsFor orbits . fst) <$> orbits

-- Part2

getOrbitingsFor :: String -> [(String, String)] -> [String]
getOrbitingsFor id' = mapMaybe (\(x, orb) -> if orb == id' then Just x else Nothing)

-- NOTE: dest means objects orbiting object
jumpsTo :: String -> String -> [(String,String)] -> [[String]]
jumpsTo loc dest orbits =
  let orbiting = getDirectOrbitsFor loc orbits
  in
  orbiting >>= recurse []
  where
    recurse :: [String] -> String -> [[String]]
    recurse acc newLoc | newLoc == dest = [acc]
                       | otherwise =
                         let orbs = getDirectOrbitsFor newLoc orbits  ++ getOrbitingsFor newLoc orbits
                         in (\x -> recurse (x: acc) x) =<< (filter (\l -> not $ l `elem` acc) orbs)

part2 :: [(String, String)] -> [Int]
part2 orbits =
  length <$> jumpsTo "YOU" "SAN" orbits

main :: IO ()
main = do
  c <- lines <$> readFile "input.txt"
  print $ take 10 c
  let orbits = parse <$> c;

  putStrLn "part1 res:"
  print $ part1 orbits
  let res2 = part2 orbits

  print $ take 10 res2

  putStrLn "part2 res:"
  print $ minimum res2 - 1
