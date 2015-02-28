module Parser where

import Data.Char
import Data.Maybe
import Prelude hiding (lookup)
import Data.Map hiding (map)

import Lexical

type Production = (String, [String])
--productions = [("LHS", ["R", "H", "S"])]

-- Shift == True
type TransitionKey = (Int, String)
type TransitionVal = (Bool, Int)
type Transition = (TransitionKey, TransitionVal)
type TransitionTable = Map TransitionKey TransitionVal
--transitions = [((4, Token Identifier "ABC"), ("shift", 5))]
data DFA = DFA {
    states :: [Int],
    units :: [AST],
    numStates :: Int,
    rules :: TransitionTable,
    productions ::[Production]
}
buildDFA :: (Int, [Transition], [Production]) -> DFA
buildDFA (ms, transitions, productions) = DFA [0] [] ms (fromList transitions) productions

data AST =
    AST {
        name :: String,
        production :: [AST]
    }
    | ASTT {
        name :: String,
        content :: (Token, TokenInfo)
    }

instance Show AST where
    show ast = name ast

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

tokenName x = error (show x)

run :: (DFA, [AST]) -> (DFA, [AST])
run (dfa, []) = (dfa, [])
run (dfa, (ast:rst)) = if isNothing lku
                        then (dfa, (ast:rst))
                        else if changeState
                                then run ((DFA (num:ss) (ast:u) ms rules prods), rst)
                                else run ((DFA nss nu ms rules prods), (nast:ast:rst))
    where
        DFA ss u ms rules prods = dfa
        tk = name ast
        s = head ss
        lku = lookup (s, tk) rules
        Just (changeState, num) = lku

        prod = prods !! num
        units = snd prod
        nast = AST (fst prod) (take (length units) u)
        nss = drop (length units) ss
        nu = drop (length units) u


buildProduction :: [String] -> Production
buildProduction (lhs:rhs) = (lhs, rhs)

buildTransition :: [String] -> Transition
buildTransition (state:token:action:next:[]) = ((read state :: Int, token), (action == "shift", read next :: Int))
buildTransition str = error (show str)

readLR1 :: IO DFA
readLR1 = do
    contentStr <- readFile "./res/joos1w.lr1"
    let content = lines contentStr
    let numOfTerminal = read (head content) :: Int
    let terminalList = take numOfTerminal (drop 1 content)
    let numOfNonterminal = read (content !! (1 + numOfTerminal)) :: Int
    let nonterminalList = take numOfNonterminal (drop (2 + numOfTerminal) content)
    let root = content !! (1 + numOfTerminal + 1 + numOfNonterminal)
    let numOfProduction = read (content !! (1 + numOfTerminal + 1 + numOfNonterminal + 1)) :: Int
    let productionList = map (buildProduction . words) (take numOfProduction (drop (1 + numOfTerminal + 1 + numOfNonterminal + 1 + 1) content))
    let numOfState = read (content !! (1 + numOfTerminal + 1 + numOfNonterminal + 1 + 1 + numOfProduction)) :: Int
    let numOfTransition = read (content !! (1 + numOfTerminal + 1 + numOfNonterminal + 1 + 1 + numOfProduction + 1)) :: Int
    let transitionList = map (buildTransition . words) (take numOfTransition (drop (1 + numOfTerminal + 1 + numOfNonterminal + 1 + 1 + numOfProduction + 1 + 1) content))
    let transitionMap = fromList(transitionList)
    --return (terminalList, nonterminalList, root, productionList, numOfState, transitionMap)
    return (DFA [0] [] numOfState transitionMap productionList)

tokenToAST :: (Token, TokenInfo) -> AST
tokenToAST (tk, ti) = ASTT (tokenName tk) (tk, ti)
