module Day8 where

import           Util

type Registry = String

data Action = Increment
            | Decrement
            deriving (Show, Read, Eq)

data Condition = Cond { reg :: Registry
                      , op  :: CompareOp
                      , arg :: Int }
                      deriving (Show)

data CompareOp = Eq
               | Leq
               | Lt
               | Geq
               | Gt
               deriving (Show, Read, Eq)

data Instruction = Inst { target :: Registry
                        , action :: Action
                        , amount :: Int
                        , cond   :: Condition }
                        deriving (Show)

day8 :: IO ()
day8 = do
    input <- getInputForDay 8
    return ()
