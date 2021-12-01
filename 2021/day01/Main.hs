{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}

input :: String -> IO [Int]
input name = do
    (fmap (read @Int) . lines) <$> readFile name

calcIncreases1 :: [Int] -> Int
calcIncreases1 = \case
    [] -> 0
    head : tail ->
        snd $ foldl calc (head, 0) tail
  where
    calc (prev, count) val =
        ( val
        , if prev < val
            then count + 1
            else count
        )

calcIncreases2 :: [Int] -> Int
calcIncreases2 = \case
    h1 : h2 : h3 : tail ->
        snd $ foldl calc ((h1, h2, h3), 0) tail
    _ -> 0
  where
    calc ((prev@(p1, p2, p3)), count) val =
        let cur = (p2, p3, val)
         in ( cur
            , if sum prev < sum cur
                then count + 1
                else count
            )
    sum (i1, i2, i3) = i1 + i2 + i3

main :: IO ()
main = do
    vals <- input "day01/input.txt"
    print $ calcIncreases1 vals
    print $ calcIncreases2 vals
