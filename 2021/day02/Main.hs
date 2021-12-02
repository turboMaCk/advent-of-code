{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

import Control.Monad

data Direction
    = F
    | D
    | U
    deriving (Show)

parseDirectionUnsafe :: String -> Direction
parseDirectionUnsafe = \case
    "forward" -> F
    "down" -> D
    "up" -> U

data Move = Move
    { dir :: Direction
    , steps :: Int
    }
    deriving (Show)

parseMoveUnsafe :: String -> Move
parseMoveUnsafe line =
    case words line of
        [dir, steps] ->
            Move (parseDirectionUnsafe dir) $ read steps

data Pos = Pos
    { z :: Int
    , y :: Int
    }
    deriving (Show)

initPos :: Pos
initPos = Pos 0 0

posToInt :: Pos -> Int
posToInt Pos{..} =
    z * y

applyMove :: Pos -> Move -> Pos
applyMove pos Move{..} =
    case dir of
        F -> pos{y = steps + (y pos)}
        D -> pos{z = steps + (z pos)}
        U -> pos{z = (z pos) - steps}

moves1 :: [Move] -> Pos
moves1 =
    foldl (applyMove) initPos

input :: String -> IO [String]
input name = do
    lines <$> readFile name

main :: IO ()
main = do
    vals <- fmap parseMoveUnsafe <$> input "day02/input.txt"
    print $ posToInt $ moves1 vals
