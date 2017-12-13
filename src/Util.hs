module Util where

import           Paths_AoC

getInputForDay :: Int -> IO String
getInputForDay n = getDataFileName $ "input" ++ show n ++ ".txt"
