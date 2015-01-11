module Lexical where

import Data.Maybe

---------------------------------------- SplitOneOf-----------------------------------------------------------------------------
splitOneOf :: (Eq a) => [a] -> [a] -> [[a]]
splitOneOf del (c:r) = h:(splitOneOf del (if t == [] then t else tail t))
    where
        pred = (\x -> elem x del)
        l = c:r
        (h, t) = break pred l
splitOneOf _ [] = []

---------------------------------------- Data Types that describe a Token-----------------------------------------------------------------------------
data TokenType = Keyword | Operator | Separator | Identifier | Comment | WhiteSpace | BoolLit | OctalLit | DecimalLit | HexLit | StringLit | NullLit | NotYetSupported | FAILURE deriving (Show, Eq)
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
tokenBuilder ("BOOL", l) = Just (Token BoolLit l)
tokenBuilder ("IDENTIFIER", l) = Just (Token Identifier l)
tokenBuilder ("KEYWORD", l) = Just (Token Keyword l)
tokenBuilder ("SEPARATOR", l) = Just (Token Separator l)
tokenBuilder ("OPERATOR", l) = Just (Token Operator l)
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
                            if (s |> x) < (s |> y) then Nothing else if (s |> x) < (s |> y) then (s |> y) else (s |> x)
                          else
                            if isJust (s |> y) then (s |> y) else (s |> x))

-- Lex is both recognizer and combinator
data Lex = Lex (String -> Maybe Token)
instance RE Lex where
    (|>) s (Lex f) = f s
    lift f = Lex f
instance Combinator Lex where

---------------------------------------- Dummies for quick check ------------------------------------------------------------------------------
dummyLex1 (s:xs) = Just (Token Identifier [s])
dummyLex1 _ = Nothing
dl1 = Lex dummyLex1

dummyLex2 (s:t:xs) = Just (Token Identifier [s, t])
dummyLex2 _ = Nothing
dl2 = Lex dummyLex2