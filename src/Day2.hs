module Day2 where

import           Control.Arrow
import           Data.List.Split
import           Data.Monoid
import           Util

day2 :: IO ()
day2 = do
    input <- getInputForDay 2
    let rows = fmap (fmap read . splitOn "\t") . lines $ input :: [[Int]]
    let minmaxes = (maximum &&& minimum) <$> rows
    let diffs = uncurry (-) <$> minmaxes
    let checksum = sum diffs
    print checksum
