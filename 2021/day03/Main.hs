{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

input :: String -> IO [String]
input name = do
    lines <$> readFile name

data Freq = Freq
    { one :: Int
    , zero :: Int
    }
    deriving (Show)

initReq :: Freq
initReq = Freq 0 0

updateFreq :: Freq -> Char -> Freq
updateFreq freq = \case
    '0' -> freq{zero = (zero freq) + 1}
    '1' -> freq{one = (one freq) + 1}

updateFreqs :: [Freq] -> [Char] -> [Freq]
updateFreqs freqs chars =
    fmap (uncurry updateFreq) $ zip freqs chars

frequencies :: [[Char]] -> [Freq]
frequencies vals =
    foldl updateFreqs (replicate ((length $ head vals)) initReq) vals

mostCommons :: [Freq] -> [Char]
mostCommons = fmap mostCommon

mostCommon :: Freq -> Char
mostCommon Freq{..} = if one >= zero then '1' else '0'

inverse :: [Char] -> [Char]
inverse = fmap inv
  where
    inv ch = if ch == '0' then '1' else '0'

toInt :: [Char] -> Int
toInt chars =
    foldl
        ( \acc (i, ch) ->
            if ch == '0'
                then acc
                else (2 ^ i) + acc
        )
        0
        $ zip [0 ..] $ reverse chars

part1 :: [String] -> Int
part1 vals =
    let gamaRate = mostCommons $ frequencies vals
        epsilonRate = inverse gamaRate
     in toInt gamaRate * toInt epsilonRate

firstBit :: [Char] -> Char
firstBit (h : _) = h

mapFirst :: (a -> b) -> (a, x) -> (b, x)
mapFirst f (a, x) =
    (f a, x)

rating2 :: (Char -> Char -> Bool) -> [String] -> [Char]
rating2 comp values = go $ fmap (\v -> (v, v)) values
  where
    go = \case
        -- We got result, return
        [(_, res)] -> res
        vals ->
            let -- ineficient
                mF = mostCommons $ frequencies $ fmap fst vals
                newVals =
                    filter
                        ( \(chars, _) ->
                            firstBit chars `comp` firstBit mF
                        )
                        vals
             in go (fmap (mapFirst (drop 1)) newVals)

part2 :: [String] -> Int
part2 vals =
    toInt (rating2 (==) vals) * toInt (rating2 (/=) vals)

main :: IO ()
main = do
    vals <- input "day03/input.txt"
    print $ part1 vals
    print $ part2 vals
