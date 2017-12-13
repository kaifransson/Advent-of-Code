module Day1 where

import           Data.Char
import           Util

match :: [Int] -> [Int]
match xs = let halfLength = length xs `div` 2
            in match' (splitAt halfLength xs) halfLength
         where match' ([], []) _ = []
               match' (x:xs, y:ys) n
                 | x == y = x:y:rest
                 | otherwise = rest
                 where rest = match' (xs, ys) (n-1)

day1 :: IO ()
day1 = do
  input <- getInputForDay 1
  print $ sum . match . fmap digitToInt $ input
