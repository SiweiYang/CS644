module Lexical where

data Token = Token { kind :: String
                   , lexeme :: String
                   , line :: Int
                   } deriving (Show, Eq)

instance Ord Token where
  tokenA `compare` tokenB = (length $ lexeme tokenA) `compare` (length $ lexeme tokenB)

tokenLength :: Token -> Int
tokenLength token = length $ lexeme token
