module Main where

import Lexical
import Scanner
import Parser

import Data.Maybe

import System.Directory

readLR :: String -> IO ([String], [String], String, Int, [[String]], [[String]])
readLR fn = do
    f <- readFile fn
    let ls = lines f
    
    --putStrLn (head ls)
    let tn = read (head ls)
    let rst = tail ls
    let ls = rst
    let terms = take tn ls
    let rst = drop tn ls
    let ls = rst
    
    --putStrLn (head ls)
    let nn = read (head ls)
    let rst = tail ls
    let ls = rst
    let nterms = take nn ls
    let rst = drop nn ls
    let ls = rst
    let root = head ls
    let rst = tail ls
    let ls = rst
    
    --putStrLn (head ls)    
    let pn = read (head ls)
    let rst = tail ls
    let ls = rst    
    
    let prods = map (splitOneOf [' ']) (take pn ls)
    let rst = drop pn ls
    let ls = rst
        
    let ms = read (head ls) :: Int
    let rst = tail ls
    let ls = rst
        
    let tn = read (head ls) :: Int
    let rst = tail ls
    let ls = rst
    
    let trans = map (splitOneOf [' ']) (take tn ls)
    return (terms, nterms, root, ms, prods, trans)

main :: IO ()
main = do
    (terms, nterms, root, ms, prods, trans) <- readLR "res/joos1w.lr1"
    --(terms, nterms, root, ms, prods, trans) <- readLR "res/sample.lr1"
    let productions = map (\terms -> (head terms, tail terms)) prods
    --let transitions = map (\[s, n, a, num] -> ((read s, n), (if a == "shift" then True else False, read num))) prods :: [Transition]
    let transitions = map (\[s, n, a, num] -> ((read s, n), (if a == "shift" then True else False, read num))) trans :: [Transition]
    putStrLn (show transitions)
    let dfa = buildDFA (ms, transitions, productions)
    return ()
    