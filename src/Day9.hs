module Day9 where

import           Control.Arrow
import           Control.Monad.State
import           Util

type Score = Int
type GarbageCount = Int
type NestLevel = Int
type Result = (Score, GarbageCount)
type StreamState = (NestLevel, GarbageCount)

score :: String -> Result
score s = evalState (score' s) (0, 0)

countGarbage :: String -> (String, GarbageCount)
countGarbage ""        = ("", 0)
countGarbage ('!':_:s) = countGarbage s
countGarbage ('>':s)   = (s, 0)
countGarbage (_:s)     = second succ $ countGarbage s

clearGarbage :: String -> State StreamState String
clearGarbage s = do
  let (s', cleared) = countGarbage s
  modify $ second (+cleared)
  return s'

decrementNest :: State StreamState ()
decrementNest = modify $ first pred

incrementNest :: State StreamState ()
incrementNest = modify $ first succ

evalPoints :: State StreamState Score
evalPoints = do
  (nestLevel, _) <- get
  decrementNest
  return nestLevel

score' :: String -> State StreamState Result
score' ""      = do
  (_, garbage) <- get
  return (0, garbage)
score' ('<':s) = clearGarbage s >>= score'
score' (',':s) = score' s
score' ('{':s) = incrementNest >> score' s
score' ('}':s) = do
  points <- evalPoints
  (points', garbage) <- score' s
  return (points + points', garbage)
score' (c:_)   = error $ "unexpected character in input: " ++ [c]

day9 :: IO ()
day9 = do
  input <- getInputForDay 9
  print $ score input
