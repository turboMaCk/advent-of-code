module Main (main) where

import Data.List

-- Vec

data Vec2 = Vec2 { vecX :: Int, vecY :: Int}
  deriving (Show, Eq)

instance Num Vec2 where
  (Vec2 x1 y1) - (Vec2 x2 y2) = Vec2 (x1-x2) (y1-y2)

instance Ord Vec2 where
  (Vec2 x1 y1) <= (Vec2 x2 y2) = (abs x1) + (abs y1) <= (abs x2) + (abs y2)

-- Euklidus' algorithm
-- A = x * B + (A `mod` b)
divisor :: Int -> Int -> Int
divisor a' b' = help (abs a') (abs b')
  where help a b | b == 0 = a
                 | a == 0 = b
                 | otherwise = divisor b (a `mod` b)

toDirection :: Vec2 -> Vec2
toDirection (Vec2 x y) = Vec2 (x `div` d) (y `div` d)
  where d = divisor x y

parseLine :: Int -> String -> [Vec2]
parseLine y = reverse . snd <$> foldl addMatch (0,[])
  where addMatch (i, acc) sym | sym == '#' = (i+1, Vec2 i y : acc)
                              | otherwise = (i+1, acc)

indexedMap :: (Int -> a -> b) -> [a] -> [b]
indexedMap f = reverse . snd <$> foldl wrap (0, [])
  where wrap (i, acc) a = (i+1, f i a : acc)

build :: [String] -> [Vec2]
build xs = concat (indexedMap parseLine xs)

uniq :: Eq a => [a] -> [a]
uniq = foldr (\x acc -> if x `elem` acc then acc else x:acc) []

collectVecs :: Vec2 -> [Vec2] -> [(Vec2, Vec2)]
collectVecs pointOfView = fmap (\object -> (object, toDirection $ object - pointOfView))

calculate1 :: [Vec2] -> [(Vec2, Int)]
calculate1 xs = fmap (\vec -> (vec, length $ uniq $ map snd $ collectVecs vec $ filter (vec /=) xs)) xs

part1 :: [Vec2] -> (Vec2, Int)
part1 = head . reverse . sortOn snd . calculate1

-- Part2

data InDir = InDir Vec2 [Vec2]
  deriving (Eq, Show)

instance Ord InDir where
  (InDir vec1 _) <= (InDir vec2 _) =
    cal vec1 <= cal vec2
    where cal (Vec2 x' y') =
            let x = fromIntegral $ abs x'
                y = fromIntegral $ abs y'
                d = sqrt (x**2 + y**2)
                (q, k) = if x' >= 0 && y' <= 0 then (0, 1)
                    else if x' > 0 && y' >= 0 then (2, 1)
                    else if x' <= 0 && y' >= 0 then (4, -1)
                    else (6, -1)
            in sin(x/d) * k + q

toInDir :: Vec2 -> [(Vec2, Vec2)] -> InDir
toInDir v vecs@((_, d):_) = InDir d $ sortOn (\o -> o - v) $ fst <$> vecs

groupVecs' :: [(Vec2, Vec2)] -> [[(Vec2, Vec2)]]
groupVecs' [] = []
groupVecs' (h@(_, d):t) =
  let (matches, others) = partition (\(_, d') -> d == d') t
  in (h:matches) : groupVecs' others

groupVecs :: Vec2 -> [Vec2] -> [InDir]
groupVecs v xs = sort $
  map (toInDir v) $ groupVecs' $ collectVecs v xs

shoot :: Int -> [InDir] -> InDir
shoot n (h:t) | n <= 1 = h
              | otherwise =
                case h of
                  InDir _ []     -> shoot n t
                  InDir d (_:t') -> shoot (n-1) (t ++ [InDir d t'])

mergeCords :: Vec2 -> Int
mergeCords (Vec2 x y) = x * 100 + y

main :: IO ()
main = do
  d <- (build . lines) <$> readFile "input.txt"
  let (bestPos, count) = part1 d
  print count

  let rounds = 200
  let positioned = groupVecs bestPos $ filter (bestPos /=) d
  let (InDir _ (res2:_)) = shoot rounds positioned
  print $ mergeCords res2
