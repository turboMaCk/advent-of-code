module Main (main) where

import qualified Debug.Trace as Debug

-- Memory

data Mem = Mem [(Int, Int)]

indexed :: [a] -> [(Int, a)]
indexed xs = reverse $ snd $ foldr (\a (i, acc) -> (i+1, (i, a):acc)) (0, []) xs

parse :: String -> Mem
parse str = Mem $ indexed $ fst $ foldl step ([], "") (str ++ ",")
  where
    step :: ([Int], String) -> Char ->  ([Int], String)
    step (res, s) ','  = (read s : res, "")
    step (res, s) d    = (res, s ++ [d])

writeMem :: Int -> Int -> Mem -> Mem
writeMem i a (Mem []) = Mem [(i, a)]
writeMem i a (Mem xs) = Mem $ writeAssoc xs
  where
    writeAssoc [] = [(i, a)]
    writeAssoc (h:t)
          | i > fst h = h : writeAssoc t
          | i == fst h = (i, a):t
          | otherwise = (i, a):h:t

readMem :: Mem -> Int -> Int
readMem (Mem []) _ = 0
readMem (Mem ((addr, val):t)) i | addr == i = val
                                | otherwise = readMem (Mem t) i

getData :: Int -> Mem -> [Int]
getData _ (Mem []) = []
getData addr (Mem mem@(h:t)) | addr == fst h = snd <$> mem
                             | addr > fst h  = getData addr (Mem t)
                             | otherwise     = []

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
    ['0','0','0','9','9'] -> Halt
    [ w , m2, m1,'0','1'] -> Add (readMod m1) (readMod m2) (writeMod w)
    [ w , m2, m1,'0','2'] -> Mul (readMod m1) (readMod m2) (writeMod w)
    ['0','0', w ,'0','3'] -> Read (writeMod w)
    ['0','0', m ,'0','4'] -> Print (readMod m)
    ['0', m2, m1,'0','5'] -> JumpIfTrue (readMod m1) (readMod m2)
    ['0', m2, m1,'0','6'] -> JumpIfFalse (readMod m1) (readMod m2)
    [ w , m2, m1,'0','7'] -> LessThan (readMod m1) (readMod m2) (writeMod w)
    [ w , m2, m1,'0','8'] -> Equals (readMod m1) (readMod m2) (writeMod w)
    ['0','0', m ,'0','9'] -> ShiftRelativeBase (readMod m)
    val        -> Debug.trace ("unknown inst: " ++ val) undefined
    where str = show int
          norm = replicate (5 - length str) '0' ++ str

data Conf m = Conf { getInput :: m Int
                   , putOutput :: Int -> m ()
                   }

go :: Monad m => Conf m -> Int -> Int -> Mem -> m Int
go conf addr rb mem =
  let (inst:rest) = getData addr mem
  in case (parseInst inst, rest) of
    (Halt, _) -> pure $ head $ getData 0 mem
    (Add r1 r2 w, a':b':t:_) ->
        go conf (addr + 4) rb $ w t ((r1 a' mem rb) + (r2 b' mem rb)) mem rb
    (Mul r1 r2 w, a':b':t:_) ->
        go conf (addr + 4) rb $ w t ((r1 a' mem rb) * (r2 b' mem rb)) mem rb
    (Read w, t':_) -> do
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
  where conf = Conf { getInput = read <$> getLine
                    , putOutput = print
                    }
