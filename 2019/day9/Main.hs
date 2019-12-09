module Main (main) where

import qualified Debug.Trace as Debug
import Data.List

-- Memory

type Mem = [(Int, Int)]

indexed :: [a] -> [(Int, a)]
indexed xs = reverse $ snd $ foldr (\a (i, acc) -> (i+1, (i, a):acc)) (0, []) xs

parse :: String -> Mem
parse str = indexed $ fst $ foldl step ([], "") (str ++ ",")
  where
    step :: ([Int], String) -> Char ->  ([Int], String)
    step (res, s) ','  = (read s : res, "")
    step (res, s) d    = (res, s ++ [d])

writeMem :: Int -> Int -> Mem -> Mem
writeMem i a mem
  | i < 0 = mem
  | otherwise = (i, a) : filter ((i /=) . fst) mem

readMem :: Mem -> Int -> Int
readMem [] _ = 0
readMem ((addr, val):t) i | addr == i = val
                          | otherwise = readMem t i

-- Int computer

type ReadMod = (Int -> Mem -> Int -> Int)
type WriteMod = (Int -> Int -> Mem -> Int -> Mem)

data Op
  = Halt
  | Add ReadMod ReadMod WriteMod
  | Mul ReadMod ReadMod WriteMod
  | Read WriteMod
  | Print ReadMod
  | JumpIfTrue ReadMod ReadMod
  | JumpIfFalse ReadMod ReadMod
  | LessThan ReadMod ReadMod WriteMod
  | Equals ReadMod ReadMod WriteMod
  | ShiftRelativeBase ReadMod

readMod :: Char -> ReadMod
readMod '0' = \addr mem _ -> mem `readMem` addr
readMod '1' = \val _ _ -> val
readMod '2' = \addr mem rb -> mem `readMem` (rb + addr)

writeMod :: Char -> WriteMod
writeMod '2' = \addr val mem rb -> writeMem (addr + rb) val mem
writeMod '0' = \addr val mem _ -> writeMem addr val mem

parseInst :: Int -> Op
parseInst int =
  case norm of
    "00099"        -> Halt
    w:m2:m1:"01"   -> Add (readMod m1) (readMod m2) (writeMod w)
    w:m2:m1:"02"   -> Mul (readMod m1) (readMod m2) (writeMod w)
    '0':'0':m:"03" -> Read (writeMod m)
    '0':'0':m:"04" -> Print (readMod m)
    '0':m2:m1:"05" -> JumpIfTrue (readMod m1) (readMod m2)
    '0':m2:m1:"06" -> JumpIfFalse (readMod m1) (readMod m2)
    w:m2:m1:"07"   -> LessThan (readMod m1) (readMod m2) (writeMod w)
    w:m2:m1:"08"   -> Equals (readMod m1) (readMod m2) (writeMod w)
    '0':'0':m:"09" -> ShiftRelativeBase (readMod m)
    val        -> Debug.trace ("unknown inst: " ++ val) undefined
    where
      norm :: String
      norm =
        if int < 10 then
          "0000" ++ show int
        else if int < 100 then
          "000" ++ show int
        else if int < 1000 then
          "00" ++ show int
        else if int < 10000 then
          "0" ++ show int
        else
          show int

data Conf = Conf { getInput :: IO Int
                 , putOutput :: Int -> IO ()
                 }

go :: Conf -> Int -> Int -> Mem -> IO Int
go conf addr rb mem =
  let (inst:rest) = snd <$> (drop addr $ sortOn fst mem)
  in case (parseInst inst, rest) of
    (Halt, _) -> pure $ snd $ head mem
    (Add r1 r2 w, a':b':t:_) ->
        go conf (addr + 4) rb $ w t ((r1 a' mem rb) + (r2 b' mem rb)) mem rb
    (Mul r1 r2 w, a':b':t:_) ->
        go conf (addr + 4) rb $ w t ((r1 a' mem rb) * (r2 b' mem rb)) mem rb
    (Read w, t':_) -> do
      putStrLn "provide input"
      val <- getInput conf
      go conf (addr + 2) rb (w t' val mem rb)
    (Print r, a':_) -> do
      let res = (r a' mem rb)
      (putOutput conf) res
      go conf (addr + 2) rb mem
    (JumpIfTrue r1 r2, a':b':_) ->
      if r1 a' mem rb /= 0 then
        go conf (r2 b' mem rb) rb mem
      else
        go conf (addr + 3) rb mem
    (JumpIfFalse r1 r2, a':b':_) ->
      if r1 a' mem rb == 0 then
        go conf (r2 b' mem rb) rb mem
      else
        go conf (addr + 3) rb mem
    (LessThan r1 r2 w, a':b':t:_) ->
      let val = if (r1 a' mem rb) < (r2 b' mem rb) then 1 else 0
      in go conf (addr + 4) rb $ w t val mem rb
    (Equals r1 r2 w, a':b':t:_) ->
      let val = if (r1 a' mem rb) == (r2 b' mem rb) then 1 else 0
      in go conf (addr + 4) rb $ w t val mem rb
    (ShiftRelativeBase r, a':_) ->
      go conf (addr + 2) (rb + (r a' mem rb)) mem

main :: IO ()
main = do
  c <- readFile "input.txt"
  go conf 0 0 (parse c) >>= print
  where
    conf = Conf { getInput = read <$> getLine
                , putOutput = print
                }
