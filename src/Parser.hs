module Parser where

import Data.Char
import Data.Maybe
import Prelude hiding (lookup)
import Data.Map hiding (map)

import Lexical

type Production = (String, [String])
--productions = [("LHS", ["R", "H", "S"])]

type TransitionKey = (Int, String)
type TransitionVal = (Bool, Int)
type Transition = (TransitionKey, TransitionVal)
transitions = [((4, Token Identifier "ABC"), ("shift", 5))]
data DFA = DFA {states :: [Int], units :: [AST], numStates :: Int, rules :: Map TransitionKey Transition, productions :: Map Int Production}

data AST = AST { name :: String, production :: [AST]} | ASTT { name :: String, content :: Token} deriving (Show)

tokenName :: Token -> String
tokenName (Token Keyword s) = "KEYWORD_" ++ (map toUpper s)
tokenName (Token Separator s) = "SEPARATOR_" ++ s
tokenName (Token Operator s) = "OPERATOR_" ++ s
tokenName (Token Identifier _) = "IDENTIFIER"

tokenName (Token BoolLit _) = "LITERAL_BOOL"
tokenName (Token DecimalLit _) = "LITERAL_INT"
tokenName (Token HexLit _) = "LITERAL_INT"
tokenName (Token CharLit _) = "LITERAL_CHAR"
tokenName (Token StringLit _) = "LITERAL_STRING"
tokenName (Token NullLit _) = "LITERAL_NULL"

tokenName _ = error "Unexpected Token"

run :: (DFA, [AST]) -> (DFA, [AST])
run (dfa, []) = (dfa, [])
run (dfa, (ast:rst)) = if isNothing lku
                        then (dfa, (ast:rst))
                        else if changeState
                                then run ((DFA (num:ss) (ast:u) ms rules prods), rst)
                                else run ((DFA (tail ss) nu ms rules prods), (ast:rst))
    where
        DFA ss u ms rules prods = dfa
        tk = name ast
        s = head ss
        lku = lookup (s, tk) rules
        Just (_, (changeState, num)) = lku
        
        prod = prods ! num
        units = snd prod
        nast = AST (fst prod) (take (length units) u)
        nu = drop (length units) u