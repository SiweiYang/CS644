module Lexical where

import Data.Maybe

import Util

---------------------------------------- Data Types that describe a Token-----------------------------------------------------------------------------
data TokenType = Keyword | Operator | Separator | Identifier | Comment | WhiteSpace | BoolLit | OctalLit | DecimalLit | HexLit | CharLit | StringLit | NullLit | NotYetSupported | FAILURE deriving (Show, Eq)
data Token = Token { tokenType :: TokenType
                   , lexeme :: String
                   } deriving (Show, Eq)
data TokenInfo = TI { file :: String
                    , ln, col :: Int
                    } deriving (Show)

instance Ord Token where
  tokenA `compare` tokenB = (length $ lexeme tokenA) `compare` (length $ lexeme tokenB)

tokenLength :: Token -> Int
tokenLength token = length $ lexeme token

---------------------------------------- Transformer from (String, String) to Token -----------------------------------------------------------------------------
tokenBuilder :: (String, String) -> Maybe Token
tokenBuilder ("IDENTIFIER", l) = Just (Token Identifier l)
tokenBuilder ("KEYWORD", l) = Just (Token Keyword l)
tokenBuilder ("SEPARATOR", l) = Just (Token Separator l)
tokenBuilder ("OPERATOR", l) = Just (Token Operator l)

tokenBuilder ("BOOL", l) = Just (Token BoolLit l)
tokenBuilder ("CHAR", l) = Just (Token CharLit l)
tokenBuilder ("STRING", l) = Just (Token StringLit l)
tokenBuilder ("DEC_INTEGER", l) = Just (Token DecimalLit l)
tokenBuilder ("NULL", l) = Just (Token NullLit l)

tokenBuilder ("COMMENT", l) = Just (Token Comment l)
tokenBuilder ("WHITESPACE", l) = Just (Token WhiteSpace l)

tokenBuilder ("ERR", l) = Just (Token FAILURE l)
tokenBuilder (_, l) = Just (Token NotYetSupported l)
---------------------------------------- Contracts to enable a combinator approach-----------------------------------------------------------------------------
class RE a where
    -- a recognizer is characterized by two functions
    -- scan for a maximal token
    -- create a recognizer from a function
    (|>) :: String -> a -> Maybe Token
    lift :: (String -> Maybe Token) -> a
    build :: (String -> Maybe (String, String)) -> a


class (RE a) => Combinator a where
    -- the ordinal combinator that takes the maximal token and break tie by ordering
    (<|>) :: a -> a -> a
    -- the equivalent combinator that takes the maximal token and fail on a tie
    (<=>) :: a -> a -> a
    x <|> y = lift (\s -> if isJust (s |> x) && isJust (s |> y)
                          then
                            if (s |> x) < (s |> y) then (s |> y) else (s |> x)
                          else
                            if isJust (s |> y) then (s |> y) else (s |> x))
    x <=> y = lift (\s -> if isJust (s |> x) && isJust (s |> y)
                          then
                            if (s |> x) == (s |> y) then Nothing else if (s |> x) < (s |> y) then (s |> y) else (s |> x)
                          else
                            if isJust (s |> y) then (s |> y) else (s |> x))

-- Lex is both recognizer and combinator
data Lex = Lex (String -> Maybe Token)
instance RE Lex where
    (|>) s (Lex f) = f s
    lift f = Lex f
    build f = lift (\s -> (f s) >>= tokenBuilder)
instance Combinator Lex where
