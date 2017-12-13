module Util where

import           Paths_AoC

getInputForDay :: Int -> IO String
getInputForDay n = do
    path <- getDataFileName $ "inputs\\input" ++ show n ++ ".txt"
    readFile path
