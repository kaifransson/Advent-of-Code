module Day3 where

import           Util

day3 :: IO ()
day3 = do
    input <- getInputForDay 3
    let target = read input :: Int
    print target
