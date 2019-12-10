module Day2 (main) where

import Data.Maybe

-- Step1

write :: Int -> a -> [a] -> [a]
write i a ls
  | i < 0 = ls
  | otherwise = next i ls
  where
    next 0 (_:xs) = a : xs
    next n (x:xs) = x : next (n-1) xs
    next _ [] = []

patch :: [Int] -> [Int]
patch (f:_:_:xs) =
  f:12:2:xs

go :: Int -> [Int] -> Int
go addr mem =
  let inst = drop addr mem
  in
    case inst of
        99:_ -> head mem
        1:a':b':t:_ ->
            let a = mem !! a'
                b = mem !! b'
            in go (addr + 4) $ write t (a + b) mem
        2:a':b':t:_ ->
            let a = mem !! a'
                b = mem !! b'
            in go (addr + 4) $ write t (a * b) mem

parse :: String -> [Int]
parse str = reverse $ fst $ foldl step ([], "") str
  where
    step :: ([Int], String) -> Char ->  ([Int], String)
    step (res, s) ','  = (read s : res, "")
    step (res, s) d    = (res, s ++ [d])

step1 :: IO Int
step1 = do
  c <- readFile "input.txt"
  pure $ go 0 $ patch $ parse c

-- Step2

patchWith :: (Int, Int) -> [Int] -> [Int]
patchWith (noun, verb) (h:_:_:xs) =
  h:noun:verb:xs

permutations :: [a] -> [b] -> [(a, b)]
permutations as bs =
  (fmap (,) as) >>= (\f -> fmap f bs)

test :: [Int] -> (Int, Int) -> Maybe (Int, Int)
test mem val =
  if go 0 (patchWith val mem) == 19690720 then
    Just val
  else
    Nothing

testAll :: [Int] -> [(Int, Int)] -> Maybe (Int, Int)
testAll mem []     = Nothing
testAll mem (h:t) =
  let res = test mem h
  in
  if isJust res then res else testAll mem t

step2 :: IO (Maybe Int)
step2 = do
  mem <- parse <$> readFile "input.txt"
  pure $ case testAll mem $ permutations [0..99] [0..99] of
    Just (noun, verb) -> Just $ 100 * noun + verb
    Nothing -> Nothing


main :: IO ()
main = do
  step1 >>= print
  step2 >>= print
