module Day8 where

import           Text.Parsec
import           Text.Parsec.String
import           Text.Parsec.Token
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

word :: Parser String
word = many $ noneOf " "

pRegistry :: Parser Registry
pRegistry = word

pAction :: Parser Action
pAction = try $
    const Increment <$> string "inc"
    <|>
    const Decrement <$> string "dec"

pOperation :: Parser CompareOp
pOperation = do
    undefined

pCond :: Parser Condition
pCond = do
    string "if"
    registry <- pRegistry
    operation <- pOperation
    argument <- undefined
    return $ Cond registry operation argument

pInstruction :: Parser Instruction
pInstruction = do
    tar <- pRegistry
    space
    act <- pAction
    space
    am <- undefined
    space
    cond <- pCond
    return $ Inst tar act am undefined

day8 :: IO ()
day8 = do
    input <- getInputForDay 8
    return ()
