module Day11 where

import           Data.Char
import           Data.List.Split
import           Paths_AoC
import           Util

data Direction = N
               | NE
               | SE
               | S
               | SW
               | NW
               deriving (Show, Read, Eq)

parseDirections :: String -> [Direction]
parseDirections input = let textDirs = fmap (fmap toUpper) . splitOn "," $ input
                         in fmap read textDirs

simplify :: [Direction] -> [Direction]
simplify [] = []

distance :: [Direction] -> Int
distance = length . simplify

day11 :: IO ()
day11 = do
    input <- getInputForDay 11
    let directions = parseDirections input
    print directions
