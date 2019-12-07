module Main (main) where

import Control.Concurrent
import Data.List
import Control.Monad

-- Int computer

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

data Conf = Conf { getInput :: IO Int
                 , putOutput :: Int -> IO ()
                 }

go :: Conf -> Int -> [Int] -> IO Int
go conf addr mem =
  let (inst:rest) = drop addr mem
  in case (parseInst inst, rest) of
    (Halt, _) -> pure $ head mem
    (Add r1 r2, a':b':t:_) ->
        go conf (addr + 4) $ write t ((r1 a' mem) + (r2 b' mem)) mem
    (Mul r1 r2, a':b':t:_) ->
        go conf (addr + 4) $ write t ((r1 a' mem) * (r2 b' mem)) mem
    (Read, t':_) -> do
      val <- getInput conf
      go conf (addr + 2) (write t' val mem)
    (Print r, a':_) -> do
      let res = (r a' mem)
      putStrLn $ "Output int: " ++ show res
      (putOutput conf) res
      go conf (addr + 2) mem
    (JumpIfTrue r1 r2, a':b':_) ->
      if r1 a' mem /= 0 then
        go conf (r2 b' mem) mem
      else
        go conf (addr + 3) mem
    (JumpIfFalse r1 r2, a':b':_) ->
      if r1 a' mem == 0 then
        go conf (r2 b' mem) mem
      else
        go conf (addr + 3) mem
    (LessThan r1 r2, a':b':t:_) ->
      let val = if (r1 a' mem) < (r2 b' mem) then 1 else 0
      in go conf (addr + 4) $ write t val mem
    (Equals r1 r2, a':b':t:_) ->
      let val = if (r1 a' mem) == (r2 b' mem) then 1 else 0
      in go conf (addr + 4) $ write t val mem

parse :: String -> [Int]
parse str = reverse $ fst $ foldl step ([], "") (str ++ ",")
  where
    step :: ([Int], String) -> Char ->  ([Int], String)
    step (res, s) ','  = (read s : res, "")
    step (res, s) d    = (res, s ++ [d])

-- Part1

mkAmp :: Chan Int -> Chan Int -> [Int] -> IO ThreadId
mkAmp rChan wChan mem = forkIO $ go conf 0 mem >>= (\_ -> writeChan wChan (-666))
  where
    conf =
      Conf
      { getInput = readChan rChan
      , putOutput = writeChan wChan
      }

type Inputs = (Int, Int, Int, Int, Int)

compute :: [Int] -> Inputs -> IO Int
compute  mem (iA, iB, iC, iD, iE) = do
  wChan <- newChan
  rChan <- newChan

  let step i1 i2 = do
        t <- mkAmp wChan rChan mem
        writeChan wChan i1
        writeChan wChan i2
        res <- readChan rChan
        killThread t
        pure res

  step iA 0 >>= step iB >>= step iC >>= step iD >>= step iE

ins :: [Int] -> [Inputs]
ins range = filter justOnce $
  pure (,,,,)
    >>= perm
    >>= perm
    >>= perm
    >>= perm
    >>= perm
  where
    perm f = fmap f range
    justOnce (a,b,c,d,e) =
      5 == length ((filter ((1 ==) . length)) $ group $ sort [a,b,c,d,e])

part1 :: [Int] -> IO Int
part1 mem = maximum <$> mapM (compute mem) (ins [0..4])

-- Part2

compute2 :: [Int] -> Inputs -> IO Int
compute2  mem (iA, iB, iC, iD, iE) = do
  let impList = [iA, iB, iC, iD, iE]

  let initAmp (wChan, acc) initInst = do
        rChan <- newChan
        amp <- mkAmp wChan rChan mem
        writeChan wChan initInst
        pure ( rChan
             , amp : acc )

  fstChan <- newChan
  (lstChan, instances) <- foldM initAmp (fstChan, []) impList

  writeChan fstChan 0

  let loop prev = do
        res <- readChan lstChan
        if res == -666 then do
            forM_ instances killThread
            pure prev
        else do
            writeChan fstChan res
            loop res

  loop 0

part2 :: [Int] -> IO Int
part2 mem = maximum <$> mapM (compute2 mem) (ins [5..9])

main :: IO ()
main = do
  c <- readFile "input.txt"
  let mem = parse c

  res1 <- part1 mem
  putStrLn $ "part1 res: " ++ show res1

  res2 <- part2 mem
  putStrLn $ "part2 res: " ++ show res2
  pure ()
