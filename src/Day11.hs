module Day11 where

import           Control.Arrow
import           Data.Char
import           Data.List.Split
import           Data.Monoid
import           Paths_AoC
import           Util

parseDirections :: String -> [Direction]
parseDirections input = let textDirs = fmap (fmap toUpper) . splitOn "," $ input
                         in fmap read textDirs

data Direction = N
               | NE
               | SE
               | S
               | SW
               | NW
               deriving (Show, Read, Eq)

phi :: Direction -> (Int, Int)
phi NE = (1, 0)
phi NW = (0, 1)
phi SE = (0, -1)
phi SW = (-1, 0)
phi N  = (1, 1)
phi S  = (-1, -1)

simplify :: [Direction] -> (Int, Int)
simplify dirs = let coordinates = fmap phi dirs
                 in (getSum *** getSum) . mconcat . fmap (Sum *** Sum) $ coordinates

distance :: [Direction] -> Int
distance dirs = let (x, y)    = simplify dirs
                    absCoords = (abs x, abs y)
                 in reduce absCoords
              where reduce (x, y)
                        | x > 0 && y > 0 = 1 + reduce (x-1, y-1)
                        | x > 0 = 1 + reduce (x-1, y)
                        | y > 0 = 1 + reduce (x, y-1)
                        | otherwise = 0

day11 :: IO ()
day11 = do
    input <- getInputForDay 11
    let directions = parseDirections input
    print $ distance directions
