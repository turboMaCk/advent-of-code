module Main (main) where

import qualified Debug.Trace as Debug

-- Int Computer

write :: Int -> a -> [a] -> [a]
write i a ls
  | i < 0 = ls
  | otherwise = next i ls
  where
    next 0 (_:xs) = a : xs
    next n (x:xs) = x : next (n-1) xs
    next _ [] = []

type ReadMod = (Int -> [Int] -> Int)

data Op
  = Halt
  | Add ReadMod ReadMod
  | Mul ReadMod ReadMod
  | Read
  | Print ReadMod
  | JumpIfTrue ReadMod ReadMod
  | JumpIfFalse ReadMod ReadMod
  | LessThan ReadMod ReadMod
  | Equals ReadMod ReadMod

readMod :: Char -> Int -> [Int] -> Int
readMod '0' = \addr mem -> mem !! addr
readMod '1' = \val _ -> val

parseInst :: Int -> Op
parseInst int =
  case norm of
    "0099"     -> Halt
    m2:m1:"01" -> Add (readMod m1) (readMod m2)
    m2:m1:"02" -> Mul (readMod m1) (readMod m2)
    "0003"     -> Read
    '0':m:"04" -> Print (readMod m)
    m2:m1:"05" -> JumpIfTrue (readMod m1) (readMod m2)
    m2:m1:"06" -> JumpIfFalse (readMod m1) (readMod m2)
    m2:m1:"07" -> LessThan (readMod m1) (readMod m2)
    m2:m1:"08" -> Equals (readMod m1) (readMod m2)
    x          -> Debug.trace ("parseInst '" ++ x ++ "' failed") undefined
    where
      norm :: String
      norm =
        if int < 10 then
          "000" ++ show int
        else if int < 100 then
          "00" ++ show int
        else if int < 1000 then
          "0" ++ show int
        else
          show int

go :: Int -> [Int] -> IO Int
go addr mem =
  let (inst:rest) = drop addr mem
  in case (parseInst inst, rest) of
    (Halt, _) -> pure $ head mem
    (Add r1 r2, a':b':t:_) ->
        go (addr + 4) $ write t ((r1 a' mem) + (r2 b' mem)) mem
    (Mul r1 r2, a':b':t:_) ->
        go (addr + 4) $ write t ((r1 a' mem) * (r2 b' mem)) mem
    (Read, t':_) -> do
      putStrLn "Input int:"
      val <- read <$> getLine
      go (addr + 2) (write t' val mem)
    (Print r, a':_) -> do
      putStrLn "Output int:"
      print (r a' mem)
      go (addr + 2) mem
    (JumpIfTrue r1 r2, a':b':_) ->
      if r1 a' mem /= 0 then
        go (r2 b' mem) mem
      else
        go (addr + 3) mem
    (JumpIfFalse r1 r2, a':b':_) ->
      if r1 a' mem == 0 then
        go (r2 b' mem) mem
      else
        go (addr + 3) mem
    (LessThan r1 r2, a':b':t:_) ->
      let val = if (r1 a' mem) < (r2 b' mem) then 1 else 0
      in go (addr + 4) $ write t val mem
    (Equals r1 r2, a':b':t:_) ->
      let val = if (r1 a' mem) == (r2 b' mem) then 1 else 0
      in go (addr + 4) $ write t val mem

parse :: String -> [Int]
parse str = reverse $ fst $ foldl step ([], "") (str ++ ",")
  where
    step :: ([Int], String) -> Char ->  ([Int], String)
    step (res, s) ','  = (read s : res, "")
    step (res, s) d    = (res, s ++ [d])

-- Part1

part2 :: IO Int
part2 = do
  c <- readFile "input.txt"
  go 0 $ parse c

main :: IO ()
main = part2 >>= print
